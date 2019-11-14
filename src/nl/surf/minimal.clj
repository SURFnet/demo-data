(ns nl.surf.minimal
  (:require [nl.surf.prop :as prop]))


(def attributes #{{:name        ::cat/id ;; entity type, name
                   :generator   `some-id
                   :constraints `[unique]}

                  ;; pietje is eigenaar van 3 katten

                  {:name ::person/description
                   :generator (fn [_] (str person/name " is eigenaar van " (count (query "select from cat_ownere where catowner.owner = my.id")) " katten"))
                   :deps [::catowner.owner ::catowner.cat]}
                  
                  {:name                     ::cat/owner
                   :generator                `select-refs ;; krijgt :deps mee als argument
                   :multiple?                true ; cat/owner wordt dan collectie [ ... ]
                   :constraints              `[(min-size 1) (max-size 9) unique-within-collection]
                   :deps                     [::person/id]}
                                        ; dus wat doen we met constraints over meerdere attributen heen?
                  
                  
                  {:name ::cat-owner/owner
                   :generator `select-ref
                   :deps [::person/id ::cat-owner/cat]
                   :constraints [(unique ::cat-owner/owner ::cat-owner/cat)]} 
                  {:name ::cat-owner/cat
                   :generator `select-ref
                   :deps [::cat/id ::cat-owner/owner]
                   :constraints [(unique ::cat-owner/owner ::cat-owner/cat)]}
                  {:name ::cat-owner/__constraints
                   
                   }

                                        ; global constraints?
                  {:global-constraints []}

                  ; bovenstaande moeten samen uniek zijn
                  
                  {:name      ::cat/name
                   :generator (fn [id] (str "Cat nr " id))
                   :deps      [::cat/id]} ;; own id or some other cat's?
                  {:name        ::person/id
                   :generator   `some-id
                   :constraints `[unique]}})

;; iedere kat heeft minstens 1 eigenaar



(def schema
  {:type1 {:range      [1 10]
           :key        :id
           :attributes {:id prop/pos-int}}})


(defn generate-entity
  [{:keys [db schema]} type]
  (let [{:keys [attributes key] :as type-schema} (get schema type)]
    (let [id-fn (or (get attributes key)
                    (throw (ex-info (str "Can't find key for type " type)
                                    {:type-schema type-schema :schema schema})))
          _     (prn id-fn)
          id    (id-fn {:db db :path [type]})]
      (assoc-in db [type id] {key id}))))

(defn generate-entities
  [schema]
  (reduce-kv
   (fn [db type {:keys [range key attributes]}]
     (let [id-fn (get attributes key)
           id    (id-fn {:db db :path [type]})]
       (assoc-in db [type id] {key id})))

   schema))
