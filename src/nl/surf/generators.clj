(ns nl.surf.generators
  (:require [clojure.data.generators :as gen])
  (:refer-clojure :exclude [int]))

(defn uuid
  []
  (fn [_]
    (gen/uuid)))

(defn string
  []
  (fn [_]
    (gen/string)))

(defn int
  []
  (fn [_]
    (gen/int)))

(defn one-of
  [coll]
  (fn [_]
    (apply gen/one-of coll)))
