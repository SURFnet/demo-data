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

(ns nl.surf.demo-data.generators
  (:refer-clojure :exclude [char int format float boolean])
  (:require [clojure.core :as core]
            [clojure.data.generators :as gen]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [remworks.markov-chain :as mc]
            [yaml.core :as yaml]))

(defn uuid
  "Build a generator to pick UUIDs."
  []
  (fn uuid [_]
    (gen/uuid)))

(defn string
  "Build a generator to pick random strings of characters."
  []
  (fn string [_]
    (gen/string)))

(defn boolean
  "Build a generator to pick random boolean values"
  []
  (fn boolean [_]
    (gen/boolean)))

(defn int
  "Build a generator to pick a integer uniformly distributed between `lo` and
  `hi`, both inclusive."
  ([]
   (fn int [_]
     (gen/int)))
  ([lo hi]
   (fn int-2[_]
     (gen/uniform lo (inc hi)))))

(defn float
  "Build a generator to pick a integer uniformly distributed between `lo` and
  `hi`, both inclusive."
  ([]
   (fn float [_]
     (gen/float)))
  ([lo hi]
   (fn float [_]
     (+ lo (* (- hi lo) (gen/float))))))

(defn bigdec-cubic
  "Build a generator to pick a long distributed between `lo` and `hi`, both
  inclusive.  This generator has cubic biased toward `hi`."
  [lo hi]
  (fn bigdec-cubic [_]
    (let [d (bigdec (- hi lo))]
      (+ lo (Math/cbrt (* (.nextDouble gen/*rnd*) (* d d d)))))))

(defn int-cubic
  "Like `bigdec-cubic` but returns an int."
  [lo hi]
  (fn int-cubic [_]
    (core/int ((bigdec-cubic lo hi) _))))

(defn int-log
  "Build a generator to pick a long distributed between `lo` and `hi`, both
  inclusive.  This generator has logarithmic biased toward `hi`."
  [lo hi]
  (fn int-log [_]
    (let [d (- hi lo)]
      (core/int (+ lo (Math/log (* (.nextDouble gen/*rnd*) (Math/exp d))))))))

(defn char
  "Build a generator to pick a character uniformly distributed between `lo` and
  `hi`, both inclusive.  Without boundaries a printable ASCII character is
  picked."
  ([]
   (fn char [_]
     (gen/printable-ascii-char)))
  ([lo hi]
   (fn char-2 [_]
     (core/char (gen/uniform (core/int lo) (inc (core/int hi)))))))

(defn one-of
  "Build a generator to pick one of `coll`."
  [coll]
  (fn one-of [_]
    (apply gen/one-of coll)))

(defn one-of-each
  "Build a generator to pick one item from each collection in `colls`."
  [colls]
  (fn one-of-each [_]
    (map #(apply gen/one-of %) colls)))

(defn weighted
  "Build a generator to pick one of weighted `m`.

  For example: with `{\"foo\" 2, \"bar\" 1}` there's a 2 in 3 chance `\"foo\"`
  will be picked."
  [m]
  (fn weighted [_]
    (gen/weighted m)))

(defn weighted-set
  "Build a generator to pick a set of weighted `m`.

  TODO FIXME"
  [m]
  (fn weighted-set [_]
    (set (repeatedly (count m) #(gen/weighted m)))))

(defn format
  "Build a generator for strings using a `fmt` as in `clojure.core/format` and
  `arg-gens` generators as arguments."
  {:deprecated "0"}
  [fmt & arg-gens]
  (fn format [world]
    (let [args (map #(% world) arg-gens)]
      (apply core/format fmt args))))

(defn object
  "Build a generator that returns a map"
  [m]
  (fn [world]
    (reduce-kv (fn [o k gen]
                 (assoc o k (gen world)))
               {}
               m)))

(defn text
  {:deprecated "0"}
  [corpus & {:keys [lines lookback] :or {lines 3, lookback 2}}]
  (let [state-space (mc/analyse-text corpus)]
    (fn text [world]
      (->> (repeatedly #(mc/generate-text state-space))
           (take lines)
           (s/join "  ")))))

;;;; Helpers

(def ^:dynamic *resource-class-loader* nil)

(defn- checked-resource
  [name]
  (or (if *resource-class-loader*
        (io/resource name *resource-class-loader*)
        (io/resource name))
      (throw (ex-info (str "Resource not found: `" name "`")
                      {:name name}))))

(defn resource
  "Return the full text from the named resource `n`."
  [n]
  (-> n checked-resource slurp))

(defn lines-resource
  "Returns a collection by reading named resource `n` and spliting lines."
  [n]
  (-> n checked-resource io/reader line-seq))

(defn yaml-resource
  "Returns YAML data from resource `n`."
  [n]
  (-> n resource (yaml/parse-string :keywords false)))
