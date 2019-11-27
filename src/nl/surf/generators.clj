(ns nl.surf.generators
  (:require [clojure.data.generators :as gen]))

(defn gen-uniq-id [_]
  (str (java.util.UUID/randomUUID)))

(def names #{"Fred" "Wilma" "Pebbles" "Dino" "Barney" "Betty" "Bamm-Bamm" "Roxy" "Hoppy" "Pearl" "Stoney" "Joe"})

(defn gen-name [_]
  (-> names shuffle first))
