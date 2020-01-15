(ns nl.surf.demo-data.export
  (:require [nl.surf.demo-data.date-util :as date-util]
            [nl.surf.demo-data.world :as world])
  (:import java.util.Calendar))

(defn export-key
  [k]
  (if (qualified-keyword? k)
    (keyword (name k))
    k))

(defn export-value
  [v]
  (cond
    (or (number? v)
        (boolean? v)
        (string? v)
        (nil? v))
    v

    (instance? Calendar v)
    (date-util/rfc3339-date v)

    (map? v)
    (into {}
          (map (fn [[k v]]
                 [(export-key k) (export-value v)])
               v))

    (coll? v)
    (mapv export-value v)

    (keyword? v)
    (name v)

    :else
    (str v)))

(defn export-entity
  [world entity {:keys [attributes pre] :as conf}]
  (let [entity (if pre (pre entity world)
                   entity)
        ks (remove #(get-in attributes [% :hidden?]) (keys entity))]
    (reduce (fn [out k]
              (assoc out (export-key k) (if (get-in attributes [k :follow-ref?])
                                          (export-entity world (world/get-entity world (get entity k)) (get-in conf [:attributes k]))
                                          (export-value (get entity k)))))
            {}
            ks)))

(defn export
  [world conf]
  (into {}
        (map (fn [[path {:keys [singleton? type] :as entity-conf}]]
               [path (if singleton?
                       (export-entity world (first (get world type)) entity-conf)
                       (map #(export-entity world % entity-conf) (get world type)))])
             conf)))
