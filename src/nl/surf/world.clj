(ns nl.surf.world)

(defn uuid []
  (java.util.UUID/randomUUID))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let [independent? (fn [attr] (-> attr :deps empty?))
      dependent?   (complement independent?)
      remove-dep   (fn [name attr] (update attr :deps #(disj (set %1) %2) name))
      pick-attr    (fn [attrs name] (->> attrs (filter #(= name (:name %))) first))]

  (defn sort-attrs
    "Sort `attrs` to ensure dependencies are met.  Uses Kahn's algorithm, see
  also: https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm"
    [attrs]
    (loop [dependent   (filter dependent? attrs)
           independent (filter independent? attrs)
           ordered     []]
      (if-let [attr (first independent)]
        (let [attrs (map (partial remove-dep (:name attr)) dependent)]
          (recur (filter dependent? attrs)
                 (into (vec (rest independent)) (filter independent? attrs))
                 (conj ordered (:name attr))))

        (if (empty? dependent)
          (map (partial pick-attr attrs) ordered)
          (throw (ex-info "circular dependency detected" {:attributes dependent})))))))

(defn get-relation
  "Get relation for `entity` from `world` of `type`."
  [{:keys [world entity]} type]
  (let [id (get entity (keyword (name type) "_id"))]
    (->> (get world type)
         (filter #(= id (:_id %)))
         first)))

(defn- pick-relation
  "Pick a random relation from `world` of `type`.

  TODO: some way to direct picking a relation"
  [world type]
  (-> world
      (get type)
      shuffle
      first
      (get :_id)))

(defn- relate-deps
  "Ensure `entity` has allow related `deps`.  `deps` are namespaced attribute
  names where the namespace determines the type.

  TODO: does not handle deps to self (will try to relate to a peer)"
  [world entity deps]
  (->> deps
       (map #(-> % namespace keyword))
       set
       (reduce (fn [entity dep-type]
                 (let [id-key (keyword (name dep-type) "_id")]
                   (if (contains? entity id-key)
                     entity
                     (assoc entity id-key (pick-relation world dep-type)))))
               entity)))

(defn- direct-dep
  "Basic case where attribute depends directly on value of dependency; no
  generator needed."
  [{{:keys [deps]} :attr :as state}]
  (let [[k] deps]
    (-> state
        (get-relation (keyword (namespace k)))
        (get k))))


(def ^:dynamic *retries* 1000)

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
      (let [val (generator state)]
        (if ((compose-constraints constraints) state val)
          val
          (if (pos? attempts)
            (recur (dec attempts))
            (throw (ex-info "Unable to satisfy constraints" {:entity (:entity state)
                                                             :attr   (:attr state)}))))))))

(defn- gen-attr
  "Generate a attribute for `entity`."
  [world entity {:keys [deps generator constraints] :as attr}]
  (let [entity    (if deps
                    (relate-deps world entity deps)
                    entity)
        generator (or generator direct-dep)
        generator (if (seq constraints)
                    (constrain generator constraints)
                    generator)]
    (assoc entity
           (:name attr)
           (generator {:entity entity
                       :attr   attr
                       :world  world}))))

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
                        (assoc m type (vec (repeatedly amount
                                                       (fn [] {:_id (uuid)})))))
                      {}
                      dist)]
    (->> attrs
         sort-attrs
         (populate world))))
