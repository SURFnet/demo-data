(ns nl.surf.demo-data.config
  (:refer-clojure :exclude [load])
  (:require [clojure.string :as s]
            [nl.surf.demo-data.constraints :as constraints]
            [nl.surf.demo-data.date-util :as date-util]
            [nl.surf.demo-data.generators :as gen]
            [nl.surf.demo-data.world :as world]
            [remworks.markov-chain :as mc]))

(defmulti load-generator :name)

(defmulti load-constraint identity)

(defn- keywordize [x]
  (cond
    (string? x)
    (apply keyword (s/split x #"/"))

    (sequential? x)
    (mapv keywordize x)

    :else
    (throw (ex-info "TODO" {:x x}))))

(defn- vectorize [x]
  (if (vector? x) x [x]))

(defn- load-dep [dep]
  (mapv keywordize (vectorize dep)))

(defn- load-attr
  [type [attr-name {:keys [hidden deps value generator constraints] :as attr}]]
  {:name      (keyword type (name attr-name))
   :hidden    hidden
   :deps      (mapv load-dep deps)
   :generator (cond
                generator
                (let [[name & args] (vectorize generator)
                      spec          {:name name, :arguments args}
                      g             (load-generator spec)]
                  (with-meta
                    (fn [{:keys [dep-vals] :as world}]
                      (let [args (concat args dep-vals)]
                        (try
                          (apply g world args)
                          (catch Throwable ex ;; FIXME catching throwable?!
                            (throw (ex-info (str ex) {:attribute attr-name
                                                      :spec      spec
                                                      :arguments args}))))))
                    spec))

                (contains? attr :value)
                (with-meta
                  (constantly value)
                  {:value value}))
   :constraints (map load-constraint constraints)})

(defn- load-unique-refs
  [type [ref-name {:keys [deps attributes unique] :as ref}]]
  (when-not (= (count deps) (count attributes))
    (throw (ex-info "Expected an attribute per dependency" {:type type, :ref ref})))

  (let [refs-name (keyword type (name ref-name))]
    (into [{:name      refs-name
            :deps      (mapv load-dep deps)
            :generator (let [args (if (sequential? unique) unique nil)
                             spec {:name      "unique-refs"
                                   :arguments args}]
                         (with-meta
                           (world/pick-unique-refs args)
                           spec))}]
          (mapv (fn [attr-name i]
                  {:name      (keyword type attr-name)
                   :deps      [[refs-name]]
                   :generator (with-meta
                                (fn ref-attr [{[refs] :dep-vals}]
                                  (prn refs)
                                  (nth refs i))
                                {:name (str (name ref-name) "/" attr-name)})})
                attributes
                (iterate inc 0)))))

(defn- load-ref
  [type [ref-name {:keys [deps unique] :as ref}]]
  (if unique
    (load-unique-refs type [ref-name ref])
    [{:name      (keyword type (name ref-name))
      :deps      (mapv load-dep deps)
      :generator (with-meta
                   (world/pick-ref)
                   {:name "ref"})}]))

(defn load
  "Load configuration and return attrs definition suitable for
  `nl.surf.demo-data.world/gen`."
  [{:keys [types]}]
  (loop [types types
         attrs #{}]
    (if-let [{:keys [name attributes refs]} (first types)]
      (recur (next types)
             (-> attrs
                 (into (map (partial load-attr name) attributes))
                 (into (mapcat (partial load-ref name) refs))))
      attrs)))

;;;;;;;;;;;;;;;;;;;;

(defmethod load-generator "constantly" [_]
  (fn constantly [_ x] x))

(defmethod load-generator "uuid" [_] (gen/uuid))

(defmethod load-generator "string" [_] (gen/string))

(defmethod load-generator "int" [_]
  (fn int
    ([world] ((gen/int) world))
    ([world lo hi] ((gen/int lo hi) world))))

(defmethod load-generator "bigdec-cubic" [_]
  (fn bigdec-cubic [world lo hi] ((gen/bigdec-cubic lo hi) world)))

(defmethod load-generator "int-cubic" [_]
  (fn int-cubic [world lo hi] ((gen/int-cubic lo hi) world)))

(defmethod load-generator "int-log" [_]
  (fn int-log [world lo hi] ((gen/int-log lo hi) world)))

(defmethod load-generator "char" [_]
  (fn char
    ([world] ((gen/char) world))
    ([world lo hi] ((gen/char (first lo) (first hi)) world))))

(defmethod load-generator "one-of" [_]
  (fn one-of [world x] ((gen/one-of x) world)))

(defmethod load-generator "one-of-resource-lines" [{[resource] :arguments}]
  (if resource
    (let [lines (gen/lines-resource resource)]
      (fn one-of-resource-lines-aot [world _]
        ((gen/one-of lines) world)))
    (fn one-of-resource-lines [world resource]
      ((gen/one-of gen/lines-resource resource) world))))

(defmethod load-generator "one-of-keyed-resource" [{[resource] :arguments}]
  (if resource
    (let [m (gen/yaml-resource resource)]
      (fn one-of-keyed-resource-aot [world _ k]
        ((gen/one-of (get m k)) world)))
    (fn one-of-keyed-resource [world resource k]
      ((gen/one-of (get (gen/yaml-resource resource) k)) world))))

(defmethod load-generator "weighted" [_]
  (fn weighted [world x] ((gen/weighted x) world)))

(defmethod load-generator "weighted-set" [_]
  (fn weighted-set [world x] ((gen/weighted-set x) world)))

(defmethod load-generator "format" [_]
  (fn format [world & args] (apply clojure.core/format args)))

(defmethod load-generator "text-from-resource" [{[resource] :arguments}]
  (if resource
    (let [state-space (mc/analyse-text (gen/resource resource))]
      (fn text-from-resource-aot [_ _]
        (mc/generate-text state-space)))
    (fn text-from-resource [_ resource]
      (let [state-space (mc/analyse-text (gen/resource resource))]
        (mc/generate-text state-space)))))

(defmethod load-generator "lorum-ipsum" [_]
  (let [state-space (mc/analyse-text (gen/resource "nl/surf/demo_data/lorum-ipsum.txt"))]
    (fn lorum-ipsum [_] (mc/generate-text state-space))))

(defmethod load-generator "inc" [_]
  (fn inc [_ v] (clojure.core/inc v)))

(defmethod load-generator "first-weekday-of" [_]
  (fn first-weekday-of [_ weekday month year]
    (date-util/nth-weekday-of 0 weekday year month)))

(defmethod load-generator "last-day-of" [_]
 (fn last-day-of [_ month year]
   (date-util/last-day-of year month)))

(defmethod load-generator "abbreviate" [_]
  (fn abbreviate [_ x]
    (->> (s/split x #"[^a-zA-Z]")
         (map first)
         (apply str)
         (s/upper-case)
         (str (when (> world/*retry-attempt-nr* 0) world/*retry-attempt-nr*)))))

;;;;;;;;;;;;;;;;;;;;

(defmethod load-constraint "unique" [_] constraints/unique)
