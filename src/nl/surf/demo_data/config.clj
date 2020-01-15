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
                          ((apply g args) world)
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
  (fn constantly [x] (clojure.core/constantly x)))

(defmethod load-generator "uuid" [_] gen/uuid)

(defmethod load-generator "string" [_] gen/string)

(defmethod load-generator "int" [_] gen/int)

(defmethod load-generator "bigdec-cubic" [_] gen/bigdec-cubic)

(defmethod load-generator "int-cubic" [_] gen/int-cubic)

(defmethod load-generator "int-log" [_] gen/int-log)

(defmethod load-generator "char" [_]
  (fn char [& args] (apply gen/char (map first args))))

(defmethod load-generator "one-of" [_] gen/one-of)

(defmethod load-generator "one-of-resource-lines" [_]
  (fn one-of-resource-lines [resource]
    (-> resource gen/lines-resource gen/one-of)))

(defmethod load-generator "one-of-keyed-resource" [{:keys [arguments]}]
  (let [m (-> arguments first gen/yaml-resource)]
    (fn one-of-keyed-resource [_ k]
      (gen/one-of (get m k)))))

(defmethod load-generator "weighted" [_] gen/weighted)

(defmethod load-generator "weighted-set" [_] gen/weighted-set)

(defmethod load-generator "format" [_]
  (fn format-generator [& args] (fn format [_] (apply clojure.core/format args))))

(defmethod load-generator "text-from-resource" [{[resource & other] :arguments}]
  (if (and resource (not other)) ;; corpus already known AOT
    (let [state-space (mc/analyse-text (gen/resource resource))]
      (fn text-from-resource-aot [_]
        (fn generate-text-aot [_] (mc/generate-text state-space))))
    (fn text-from-resource [resource]
      (let [state-space (mc/analyse-text (gen/resource resource))]
        (fn generate-text [_] (mc/generate-text state-space))))))

(defmethod load-generator "lorum-ipsum" [_]
  (let [state-space (mc/analyse-text (gen/resource "nl/surf/demo_data/lorum-ipsum.txt"))]
    (fn []
      (fn lorum-ipsum [_] (mc/generate-text state-space)))))

(defmethod load-generator "inc" [_]
  (fn [v]
    (fn inc [_] (clojure.core/inc v))))

(defmethod load-generator "first-weekday-of" [_]
  (fn [weekday month year]
    (fn first-weekday-of [_]
      (date-util/nth-weekday-of 0 weekday year month))))

(defmethod load-generator "last-day-of" [_]
  (fn [month year]
    (fn last-day-of [_]
      (date-util/last-day-of year month))))

(defmethod load-generator "abbreviate" [_]
  (fn [x]
    (fn abbreviate [_]
      (->> (s/split x #"[^a-zA-Z]")
           (map first)
           (apply str)
           (s/upper-case)
           (str (when (> world/*retry-attempt-nr* 0) world/*retry-attempt-nr*))))))

;;;;;;;;;;;;;;;;;;;;

(defmethod load-constraint "unique" [_] constraints/unique)
