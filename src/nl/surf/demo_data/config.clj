;; Copyright (C) 2020 SURFnet B.V.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see http://www.gnu.org/licenses/.

(ns nl.surf.demo-data.config
  (:refer-clojure :exclude [load])
  (:require [clojure.string :as s]
            [nl.surf.demo-data.constraints :as constraints]
            [nl.surf.demo-data.date-util :as date-util]
            [nl.surf.demo-data.generators :as gen]
            [nl.surf.demo-data.world :as world]
            [remworks.markov-chain :as mc]))

(defmulti generator
  "Load generator function for given spec.  Spec has a `:name` (used for
  dispatch) and `:arguments`.  The later can be used to prepare a generator
  function for action.  The returned function accepts a `world` argument,
  supplied arguments and dependency values."
  :name)

(defmulti constraint
  "Load constraint function for given name."
  identity)

(defn- keywordize-deps [x]
  (cond
    (string? x)
    (apply keyword (s/split x #"/"))

    (sequential? x)
    (mapv keywordize-deps x)

    :else
    (throw (ex-info "Unexpected dependency value" {:value x}))))

(defn- vectorize [x]
  (if (vector? x) x [x]))

(defn- load-dep [dep]
  (mapv keywordize-deps (vectorize dep)))

(defn- load-attr
  [type [attr-name {:keys [deps value constraints] gnrtr :generator :as attr}]]
  (assoc attr
         :name      (keyword type (name attr-name))
         :deps      (mapv load-dep deps)
         :generator (cond
                      gnrtr
                      (let [[name & args] (vectorize gnrtr)
                            spec          {:name name, :arguments args}
                            g             (generator spec)]
                        (with-meta
                          (fn [{:keys [dep-vals] :as world}]
                            (let [args (concat args dep-vals)]
                              (try
                                (apply g world args)
                                (catch Exception ex
                                  (throw (ex-info (str ex)
                                                  {:attribute attr-name
                                                   :generator spec
                                                   :arguments (into [world] args)}
                                                  ex))))))
                          spec))

                      (contains? attr :value)
                      (with-meta
                        (constantly value)
                        {:value value}))
         :constraints (map constraint constraints)))

(defn- load-unique-refs
  [type [ref-name {:keys [deps attributes unique hidden] :as ref :or {hidden true}}]]
  (when-not (= (count deps) (count attributes))
    (throw (ex-info "Expected an attribute per dependency" {:type type, :ref ref})))

  (let [refs-name (keyword type (name ref-name))]
    (into [{:name      refs-name
            :hidden    hidden
            :deps      (mapv load-dep deps)
            :generator (let [args (if (sequential? unique) unique nil)
                             spec {:name      "unique-refs"
                                   :arguments args}]
                         (with-meta
                           (world/pick-unique-refs args)
                           spec))}]
          (mapv (fn [attr-name i]
                  {:name      (keyword type attr-name)
                   :hidden    hidden
                   :deps      [[refs-name]]
                   :generator (with-meta
                                (fn ref-attr [{[refs] :dep-vals}]
                                  (nth refs i))
                                {:name (str (name ref-name) "/" attr-name)})})
                attributes
                (iterate inc 0)))))

(defn- load-ref
  [type [ref-name {:keys [deps hidden unique] :as ref :or {hidden true}}]]
  (if unique
    (load-unique-refs type [ref-name ref])
    [{:name      (keyword type (name ref-name))
      :hidden    hidden
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

(defmethod generator "constantly" [_]
  (fn constantly [_ x] x))

(defmethod generator "uuid" [_] (gen/uuid))

(defmethod generator "string" [_] (gen/string))

(defmethod generator "int" [_]
  (fn int
    ([world] ((gen/int) world))
    ([world lo hi]
     (when-not (< lo hi) (throw (ex-info "Expected lo < hi" {:lo lo, :hi hi})))
     ((gen/int lo hi) world))))

(defmethod generator "bigdec-cubic" [_]
  (fn bigdec-cubic [world lo hi]
    (when-not (< lo hi) (throw (ex-info "Expected lo < hi" {:lo lo, :hi hi})))
    ((gen/bigdec-cubic lo hi) world)))

(defmethod generator "int-cubic" [_]
  (fn int-cubic [world lo hi]
    (when-not (< lo hi) (throw (ex-info "Expected lo < hi" {:lo lo, :hi hi})))
    ((gen/int-cubic lo hi) world)))

(defmethod generator "int-log" [_]
  (fn int-log [world lo hi]
    (when-not (< lo hi) (throw (ex-info "Expected lo < hi" {:lo lo, :hi hi})))
    ((gen/int-log lo hi) world)))

(defmethod generator "char" [_]
  (fn char
    ([world] ((gen/char) world))
    ([world lo hi]
     (let [lo (first lo)
           hi (first hi)]
       (when-not (< (int lo) (int hi)) (throw (ex-info "Expected lo < hi" {:lo lo, :hi hi})))
       ((gen/char lo hi) world)))))

(defmethod generator "one-of" [_]
  (fn one-of [world x]
    (when (seq x) ((gen/one-of x) world))))

(defmethod generator "one-of-resource-lines" [{[resource] :arguments}]
  (if resource
    (let [lines (gen/lines-resource resource)]
      (fn one-of-resource-lines-aot [world _]
        (when (seq lines) ((gen/one-of lines) world))))
    (fn one-of-resource-lines [world resource]
      (when-let [lines (seq (gen/lines-resource resource))]
        ((gen/one-of lines) world)))))

(defmethod generator "one-of-keyed-resource" [{[resource] :arguments}]
  (if resource
    (let [m (gen/yaml-resource resource)]
      (fn one-of-keyed-resource-aot [world _ k]
        (when-let [x (seq (get m k))]
          ((gen/one-of x) world))))
    (fn one-of-keyed-resource [world resource k]
      (when-let [x (seq (get (gen/yaml-resource resource) k))]
        ((gen/one-of x) world)))))

(defmethod generator "weighted" [_]
  (fn weighted [world x]
    (when (seq x) ((gen/weighted x) world))))

(defmethod generator "weighted-set" [_]
  (fn weighted-set [world x]
    (when (seq x) ((gen/weighted-set x) world))))

(defmethod generator "format" [_]
  (fn format [world fmt & args]
    (apply clojure.core/format fmt args)))

(defmethod generator "text-from-resource" [{[resource] :arguments}]
  (if resource
    (let [state-space (mc/analyse-text (gen/resource resource))]
      (fn text-from-resource-aot [_ _]
        (mc/generate-text state-space)))
    (fn text-from-resource [_ resource]
      (let [state-space (mc/analyse-text (gen/resource resource))]
        (mc/generate-text state-space)))))

(defmethod generator "lorum-ipsum" [_]
  (let [state-space (mc/analyse-text (gen/resource "nl/surf/demo_data/lorum-ipsum.txt"))]
    (fn lorum-ipsum [_ & [lines]]
      (->> #(mc/generate-text state-space)
           (repeatedly (or lines 3))
           (s/join " ")))))

(defmethod generator "inc" [_]
  (fn inc [_ v] (clojure.core/inc v)))

(defmethod generator "first-weekday-of" [_]
  (fn first-weekday-of [_ weekday month year]
    (date-util/nth-weekday-of 0 weekday year month)))

(defmethod generator "last-day-of" [_]
 (fn last-day-of [_ month year]
   (date-util/last-day-of year month)))

(defmethod generator "abbreviate" [_]
  (fn abbreviate [_ x]
    (->> (s/split x #"[^a-zA-Z]")
         (map first)
         (apply str)
         (s/upper-case)
         (str (when (> world/*retry-attempt-nr* 0) world/*retry-attempt-nr*)))))

(defmethod generator "object" [_]
  (fn object [world & keys-n-args]
    (when-not (even? (count keys-n-args))
      (ex-info "Expected even amount of arguments" {:args keys-n-args}))
    (let [n (/ (count keys-n-args) 2)]
      (apply hash-map (mapcat (fn [k v] [k v])
                              (take n keys-n-args)
                              (drop n keys-n-args))))))

(defmethod generator "join" [_]
  (fn join [world & xs]
    (->> xs (filter identity) (s/join " "))))

(defmethod generator "date" [_]
  (fn date [world lo hi]
    (let [lo (date-util/->msecs-since-epoch (date-util/parse-date lo))
          hi (date-util/->msecs-since-epoch (date-util/parse-date hi))]
      (date-util/<-msecs-since-epoch ((gen/int lo hi) world)))))

(defmethod generator "timestamp" [_]
  (fn date [world lo hi]
    (let [lo (date-util/->msecs-since-epoch (date-util/parse-timestamp lo))
          hi (date-util/->msecs-since-epoch (date-util/parse-timestamp hi))]
      (.toInstant (date-util/<-msecs-since-epoch ((gen/int lo hi) world))))))

;;;;;;;;;;;;;;;;;;;;

(defmethod constraint "unique" [_] constraints/unique)
