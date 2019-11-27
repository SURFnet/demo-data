(ns nl.surf.generators
  (:require [clojure.data.generators :as gen]
            [clojure.spec.alpha :as s])
  (:refer-clojure :exclude [int]))

(s/def ::name qualified-keyword?)
(s/def ::deps (s/coll-of ::name))
(s/def ::constraints (s/coll-of fn?))

(s/def ::attr (s/keys :req-un [::name ::generator]
                      :opt-un [::deps ::constraints]))

(s/def ::world (s/map-of keyword? (s/coll-of ::entity)))

(s/def ::gen-state (s/keys :req-un [::entity ::attr ::world]))

(s/def ::generator (s/fspec :args (s/cat :state ::gen-state)))

(s/def ::_id uuid?)
(s/def ::entity (s/keys :opt-un [::_id]))

(defn uuid
  [_]
  (gen/uuid))

(defn string
  [_]
  (gen/string))

(defn int
  [_]
  (gen/int))

(defn one-of
  [coll]
  (fn [_]
    (apply gen/one-of coll)))

;; constraints

(defn unique
  [{:keys [world attr]} val]
  (let [entity-type (-> attr :name namespace keyword)]
    (not-any? #(= % val) (map (:name attr) (get world entity-type)))))
