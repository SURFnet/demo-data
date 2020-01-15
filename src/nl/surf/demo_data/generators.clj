(ns nl.surf.demo-data.generators
  (:refer-clojure :exclude [char int format])
  (:require [clojure.core :as core]
            [clojure.data.generators :as gen]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [remworks.markov-chain :as mc]
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

(defn bigdec-cubic
  "Build a generator to pick a long distributed between `lo` and `hi`, both
  inclusive.  This generator has cubic biased toward `hi`."
  [lo hi]
  (fn [_]
    (let [d (bigdec (- hi lo))]
      (+ lo (Math/cbrt (* (.nextDouble gen/*rnd*) (* d d d)))))))

(defn int-cubic
  "Like `bigdec-cubic` but returns an int."
  [lo hi]
  (fn [_]
    (core/int ((bigdec-cubic lo hi) _))))

(defn int-log
  "Build a generator to pick a long distributed between `lo` and `hi`, both
  inclusive.  This generator has logarithmic biased toward `hi`."
  [lo hi]
  (fn [_]
    (let [d (- hi lo)]
      (core/int (+ lo (Math/log (* (.nextDouble gen/*rnd*) (Math/exp d))))))))

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

(defn one-of-each
  "Build a generator to pick one item from each collection in `colls`."
  [colls]
  (fn [_]
    (map #(apply gen/one-of %) colls)))

(defn weighted
  "Build a generator to pick one of weighted `m`.

  For example: with `{\"foo\" 2, \"bar\" 1}` there's a 2 in 3 chance `\"foo\"`
  will be picked."
  [m]
  (fn [_]
    (gen/weighted m)))

(defn weighted-set
  "Build a generator to pick a set of weighted `m`.

  TODO FIXME"
  [m]
  (fn [_]
    (set (repeatedly (count m) #(gen/weighted m)))))

(defn format
  "Build a generator for strings using a `fmt` as in `clojure.core/format` and
  `arg-gens` generators as arguments."
  [fmt & arg-gens]
  (fn [world]
    (let [args (map (fn [gen] (gen world)) arg-gens)]
      (apply core/format fmt args))))

(defn text
  [corpus & {:keys [lines lookback] :or {lines 3, lookback 2}}]
  (let [state-space (mc/analyse-text corpus)]
    (fn [world]
      (->> (repeatedly #(mc/generate-text state-space))
           (take lines)
           (s/join "  ")))))

;;;; Helpers

(defn- checked-resource
  [name]
  (or (io/resource name)
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
