(ns nl.surf.demo-data.world
  (:require [clojure.data.generators :as data.generators]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as s]))

(defn- ref?
  [prop]
  (and (vector? prop)
       (= 2 (count prop))
       (qualified-keyword? (first prop))))

(defn- join?
  [prop]
  (and (ref? prop)
       (qualified-keyword? (second prop))))

(let [flatten-dep  (fn [dep]
                     (mapcat #(if (vector? %) % [%]) dep))
      flatten-deps (fn [attr] (assoc attr :flat-deps (set (mapcat flatten-dep (:deps attr)))))
      independent? (fn [attr] (-> attr :flat-deps empty?))
      dependent?   (complement independent?)
      remove-dep   (fn [name attr] (update attr :flat-deps #(disj (set %1) %2) name))
      pick-attr    (fn [attrs name] (->> attrs (filter #(= name (:name %))) first))]

  (defn sort-attrs
    "Sort `attrs` to ensure dependencies are met.  Uses Kahn's algorithm, see
  also: https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm"
    [attrs]
    (let [attrs* (map flatten-deps attrs)]
      (doseq [dep (mapcat :flat-deps attrs*)]
        (when-not (some #(= (:name %) dep) attrs*)
          (throw (ex-info (str "dependency on undefined attribute " dep) {:dep dep}))))

      (loop [dependent   (filter dependent? attrs*)
             independent (filter independent? attrs*)
             ordered     []]
        (if-let [attr (first independent)]
          (let [attrs* (map (partial remove-dep (:name attr)) dependent)]
            (recur (filter dependent? attrs*)
                   (into (vec (rest independent)) (filter independent? attrs*))
                   (conj ordered (:name attr))))

          (if (empty? dependent)
            (map (partial pick-attr attrs) ordered)
            (throw (ex-info "circular dependency detected" {:attributes dependent}))))))))

(defn- values
  "All values in world for attribute `attr-name`"
  [world attr-name]
  (let [entity-type (-> attr-name namespace keyword)]
    (keep attr-name (entity-type world))))

(defn pick-ref
  "Select a random reference to an attribute (from deps) in world"
  []
  (fn [{:keys [world] {:keys [name deps]} :attr}]
    (when-not (and (= 1 (count deps) (count (first deps))))
      (throw (ex-info (str "Need exactly one direct dependency to create reference for " name)
                      {:deps deps
                       :name name})))
    (let [[[ref-type]] deps]
      [ref-type (apply data.generators/one-of (values world ref-type))])))

(defn pick-unique-ref
  "Select a random attribute-value tuple (from deps) that hasn't been
  used for the current attribute."
  []
  (fn [{:keys [world] {:keys [name deps]} :attr}]
    (when-not (and (= 1 (count deps) (count (first deps))))
      (throw (ex-info (str "Need exactly one direct dependency to create reference for " name)
                      {:deps deps
                       :name name})))
    (let [[[ref-type]] deps
          taken        (set (map second (values world name)))
          free         (remove taken (values world ref-type))]
      (when (empty? free)
        (throw (ex-info (str "No unique refs to " ref-type " available for " name)
                        {:name     name
                         :ref-type ref-type})))
      [ref-type (apply data.generators/one-of free)])))


(defn get-entities
  "Get all entities with the given attribute - value pair"
  [world [attr-name value]]
  (let [entity-type (-> attr-name namespace keyword)]
    (filter #(= value (attr-name %)) (entity-type world))))

(defn get-entity
  "Get first entity with the given ref (attribute - value pair; assumed to be unique)"
  [world ref]
  (first (get-entities world ref)))

(let [combinations (fn [world names]
                     (apply combo/cartesian-product (map (fn [name]
                                                           (mapv (fn [val]
                                                                   [name val])
                                                                 (values world name)))
                                                         names)))]
  (defn pick-unique-refs
    "Select a combination of refs (from deps) that's unique for this attribute"
    ([]
     (pick-unique-refs nil))
    ([at-least-once]
     {:pre [(or (nil? at-least-once)
                (vector? at-least-once))]}
     (fn [{:keys [world] {:keys [name deps]} :attr}]
       (when-not (every? #(= 1 (count %)) deps)
         (throw (ex-info (str "Need only direct dependencies to create references for " name)
                         {:deps deps
                          :name name})))
       (let [ref-types  (map first deps)
             taken      (set (values world name))
             free       (remove taken (combinations world ref-types))
             ;; combinations are preferred when none of their refs
             ;; marked as `at-least-once` are already taken
             ref-taken? (fn ref-taken? [i ref]
                          (boolean (some #(= ref (nth % i)) taken)))
             preferred? (fn preferred? [refs]
                          (loop [[ref & refs]                     refs
                                 [at-least-once? & at-least-once] at-least-once
                                 i                                0]
                            (cond
                              (and at-least-once? (ref-taken? i ref))
                              false

                              (empty? refs)
                              true

                              :else
                              (recur refs at-least-once (inc i)))))
             preferred (when (some true? at-least-once)
                         (filter preferred? free))]
         (when (empty? free)
           (throw (ex-info (str "No unique refs to " (s/join ", " ref-types) " available for " name)
                           {:name      name
                            :ref-types ref-types})))
         (vec (apply data.generators/one-of (or (seq preferred) free))))))))

(defn lookup-path
  "Recursively lookup a value from path starting from an entity."
  [world entity path]
  (let [prop (first path)]
    (if (join? prop)
      (mapv #(lookup-path world % (next path))
            (get-entities world [(first prop) [(second prop) (or (get entity (second prop))
                                                                 (throw (ex-info (str "Cannot join on nil value for " (second prop))
                                                                                 {:entity entity
                                                                                  :join   prop
                                                                                  :path   path})))]]))
      (let [value (get entity prop)]
        (if-let [path (next path)]
          (recur world (get-entity world value) path)
          value)))))

(def ^:dynamic *retries* 1000)
(def ^:dynamic *retry-attempt-nr* 0)

(defn- compose-constraints
  "Compose a collection of constraints into a single constraint

  Will short-circuit on the first unsatisfied constraint."
  [constraints]
  (fn [state val]
    (reduce
     (fn [_ constraint]
       (or (constraint state val) (reduced false)))
     true constraints)))

(defn- constrain
  "Constrain `generator` with `constraints`

  generator will be called `*retries*` times until its return `val`
  satisfies `(constraint generator-state val)` for every constraint in
  `constaints.

  If no satisfying value was generated an exception will be thrown."
  [generator constraints]
  (fn [state]
    (loop [attempts *retries*]
      (let [val (binding [*retry-attempt-nr* (- *retries* attempts)]
                  (generator state))]
        (if ((compose-constraints constraints) state val)
          val
          (if (pos? attempts)
            (recur (dec attempts))
            (throw (ex-info "Unable to satisfy constraints" {:entity (:entity state)
                                                             :attr   (:attr state)}))))))))

(defn- copying-generator
  [{:keys [deps name]}]
  (when-not (= 1 (count deps))
    (throw (ex-info (str "Need exactly one dependency to create copying generator for " name)
                    {:name name
                     :deps deps})))
  (fn [{:keys [dep-vals]}]
    (first dep-vals)))

(defn- gen-attr
  "Generate a attribute for `entity`."
  [world entity {:keys [deps generator constraints name] :as attr}]
  (let [generator (or generator (copying-generator attr))
        generator (if (seq constraints)
                    (constrain generator constraints)
                    generator)]
    (assoc entity
           name
           (generator {:entity   entity
                       :attr     attr
                       :world    world
                       :dep-vals (map (partial lookup-path world entity) deps)}))))

(defn- gen-attrs
  "Generate all properties for `attr`"
  [world attr]
  (let [entity-type (-> attr :name namespace keyword)]
    ;; slightly convoluted because we need every call to `gen-attr` to
    ;; have access to the full world-state up till now, including
    ;; previous calls to `gen-attr` for the same attribute (earlier
    ;; entities) to make it possible to implement uniqueness as a
    ;; constraint.
    (reduce (fn [w i]
              (update-in w [entity-type i]
                         #(gen-attr w % attr)))
            world
            (range 0 (count (get world entity-type))))))

(defn- populate
  "Populate all `attrs` in `world` where `world` is a map for types to lists of
  entities."
  [world attrs]
  (reduce gen-attrs world attrs))

(defn gen
  "Generate a world given `attrs` and `dist`"
  [attrs dist]
  ;; TODO: check that keys in dist occur in attrs namespaces
  (let [world (reduce (fn [m [type amount]]
                        ;; entities needs to be a vector so we can
                        ;; update-in specific entities
                        (assoc m type (vec (repeat amount {}))))
                      {}
                      dist)]
    (->> attrs
         sort-attrs
         (populate world))))
