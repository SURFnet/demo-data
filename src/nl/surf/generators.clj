(ns nl.surf.generators
  (:refer-clojure :exclude [char int format])
  (:require [clojure.core :as core]
            [clojure.data.generators :as gen]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [yaml.core :as yaml]))

(defn uuid
  "Build a generator to pick UUIDs."
  []
  (fn [_]
    (gen/uuid)))

(defn string
  "Build a generator to pick random strings of characters."
  []
  (fn [_]
    (gen/string)))

(defn int
  "Build a generator to pick a integer uniformly distributed between `lo` and
  `hi`, both inclusive."
  ([]
   (fn [_]
     (gen/int)))
  ([lo hi]
   (fn [_]
     (gen/uniform lo (inc hi)))))

(defn char
  "Build a generator to pick a character uniformly distributed between `lo` and
  `hi`, both inclusive.  Without boundaries a printable ASCII character is
  picked."
  ([]
   (fn [_]
     (gen/printable-ascii-char)))
  ([lo hi]
   (fn [_]
     (core/char (gen/uniform (core/int lo) (inc (core/int hi)))))))

(defn one-of
  "Build a generator to pick one of `coll`."
  [coll]
  (fn [_]
    (apply gen/one-of coll)))

(defn format
  "Build a generator for strings using a `fmt` as in `clojure.core/format` and
  `arg-gens` generators as arguments."
  [fmt & arg-gens]
  (fn [world]
    (let [args (map (fn [gen] (gen world)) arg-gens)]
      (apply core/format fmt args))))

;;;; Helpers

(defn lines-resource
  "Returns a collection by reading named resource `n` and spliting lines."
  [n]
  (-> n io/resource slurp s/split-lines))

(defn yaml-resource
  "Returns YAML data from resource `n`."
  [n]
  (-> n io/resource slurp (yaml/parse-string :keywords false)))
