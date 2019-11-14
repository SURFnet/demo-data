(ns nl.surf.prop
  (:require [clojure.data.generators :as gen]))

;; Shape of generators:
;;
;; (fn [{:keys [db path] :as context} args]) => value
;;

(def ^:dynamic *retries* 1000)

(defn constrain
  [generator & constraints]
  (fn [{:keys [path] :as context}]
    (loop [attempts *retries*]
      (let [val (generator context)]
        (if ((apply every-pred constraints) context val)
          val
          (if (pos? attempts)
            (recur (dec attempts))
            (throw (ex-info "Unable to satisfy constraint" {:path path}))))))))

(defn primitive-constraint
  [pred]
  (fn [_ val]
    (pred val)))

(def pos-int
  (constrain (fn [_] (gen/int))
             (primitive-constraint pos?)))

(defn subset
  ([coll]
   (subset coll 0 (count coll)))
  ([coll min-size]
   (subset coll min-size (count coll)))
  ([coll min-size max-size]
   {:pre [(<= min-size max-size)]}
   (fn [_]
     (let [amount (+ min-size (rand-int (inc max-size)))]
       (if (zero? amount)
         (empty coll)
         (into (empty coll) (gen/reservoir-sample amount coll)))))))

