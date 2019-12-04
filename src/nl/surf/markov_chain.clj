(ns nl.surf.markov-chain
  "Implementation of Markov Chain to generate, for instance, text."
  (:require [clojure.data.generators :as gen]
            [clojure.string :as s]))

(defn- build-line-state-space [words lookback]
  (reduce (fn [m i]
            (let [k (if (<= i lookback)
                      (->> words (take i))
                      (->> words (drop (- i lookback)) (take lookback)))]
              (update m k (fn [v] ((fnil conj []) v (when (< i (count words)) (nth words i)))))))
          {}
          (range (+ (count words) lookback))))

(defn build-state-space
  "Bujld a state space using `corpus` a vector of chains.  Option `lookback`
  determines the strength of the lookup key and defaults to 2."
  [corpus & {:keys [lookback max-length] :or {lookback 2}}]
  {:space      (reduce (fn [m line]
                         (merge-with into m (build-line-state-space line lookback)))
                       {}
                       corpus)
   :lookback   lookback
   :max-length (or max-length (apply max (map count corpus)))})

(defn generate
  "Generate new chains using the probabilities described in `state-space`."
  [{:keys [space lookback max-length] :as state-space}]
  (let [max-length (or max-length (:max-length state-space))]
    (loop [r [], n 0]
      (let [c (->> r reverse (take lookback) reverse)
            w (-> space (get c) gen/shuffle first)]
        (if (< n max-length)
          (if w
            (recur (conj r w) (inc n))
            r)
          (if w
            (into r [::et-cetera])
            r))))))

(defn- split-lines [text]
  (map s/trim (re-seq #".*?[.?!]\s" (s/replace text #"\s+" " "))))

(defn- split-words [line]
  (-> line (s/split #"\s+")))

(defn build-text-state-space
  "Build a state space using `text` as a corpus of lines and words."
  [text & {:keys [lookback] :or {lookback 2}}]
  (build-state-space (->> text split-lines (map split-words)) :lookback lookback))
