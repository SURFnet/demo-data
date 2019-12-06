(ns nl.surf.markov-chain
  "Implementation of Markov Chain to generate, for instance, text."
  (:require [clojure.data.generators :as gen]
            [clojure.string :as s]))

(defn- analyse-line [words lookback]
  (reduce (fn [m i]
            (let [k (if (<= i lookback)
                      (->> words (take i))
                      (->> words (drop (- i lookback)) (take lookback)))]
              (update m k (fn [v] ((fnil conj []) v (when (< i (count words)) (nth words i)))))))
          {}
          (range (+ (count words) lookback))))

(defn analyse
  "Analyse `corpus` and return a state space.  Option `lookback` determines the
  strength of the lookup key and defaults to 2."
  [corpus & {:keys [lookback max-length] :or {lookback 2}}]
  {:space      (reduce (fn [m line]
                         (merge-with into m (analyse-line line lookback)))
                       {}
                       corpus)
   :lookback   lookback
   :max-length (or max-length (apply max (map count corpus)))})

(defn generate
  "Generate new chains from given `state-space`."
  [{:keys [space lookback max-length] :as state-space}]
  (let [max-length (or max-length (:max-length state-space))]
    (loop [r [], n 0]
      (let [c (->> r reverse (take lookback) reverse)
            w (apply gen/one-of (get space c))]
        (if (< n max-length)
          (if w
            (recur (conj r w) (inc n))
            r)
          (if w
            (into r [::et-cetera])
            r))))))

(defn split-sentences
  "Split `text` into sentences."
  [text]
  (map s/trim (re-seq #".*?[.?!](?:\s|$)" (s/replace text #"\s+" " "))))

(defn split-words
  "Split `sentence` into words."
  [sentence]
  (s/split sentence #"\s+"))

(defn analyse-text
  "Analyse `text` as a corpus of lines and words."
  [text & opts]
  (apply analyse (->> text split-sentences (map split-words)) opts))

(defn generate-text
  "Generate a new sentence from are `state-space`."
  [state-space]
  (->> state-space generate (s/join " ")))
