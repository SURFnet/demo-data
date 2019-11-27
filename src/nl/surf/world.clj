(ns nl.surf.world)

(defn uuid []
  (java.util.UUID/randomUUID))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sort-attrs
  "Sort `attrs` to ensure dependencies are met."
  [attrs]
  (sort (fn [a b]
          (cond
            ;; `a` depends on `b`
            ((set (:deps a)) (:name b))
            1

            ;; `b` depends on `a`
            ((set (:deps b)) (:name a))
            -1

            :else
            0))
        attrs))

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

(defn- gen-attr
  "Generate a attribute for `entity`."
  [world entity {:keys [deps generator] :as attr}]
  (let [entity (if deps
                 (relate-deps world entity deps)
                 entity)
        generator (or generator direct-dep)]

    (assoc entity
           (:name attr)
           (generator {:entity entity
                       :attr   attr
                       :world  world}))))

(defn- populate
  "Populate all `attrs` in `world` where `world` is a map for types to lists of
  entities."
  [world attrs]
  (reduce (fn [world attr]
            (update world (-> attr :name namespace keyword)
                    #(map (fn [ent]
                            (gen-attr world ent attr))
                          %)))
          world
          attrs))

(defn gen
  "Generate a world given `attrs` and `dist`"
  [attrs dist]
  ;; TODO: check that keys in dist occur in attrs namespaces
  (let [world (reduce (fn [m [type amount]]
                        (assoc m type (repeatedly amount
                                                  (fn [] {:_id (uuid)}))))
                      {}
                      dist)]
    (->> attrs
         sort-attrs
         (populate world))))
