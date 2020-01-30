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

(ns nl.surf.demo-data
  (:gen-class)
  (:require [cheshire.core :as json]
            [nl.surf.demo-data.bootstrap :as bootstrap]
            [nl.surf.demo-data.config :as config]
            [nl.surf.demo-data.world :as world]))

(defn bootstrap
  [in schema-path & [population-path]]
  (let [schema     (bootstrap/spec->types (json/parse-string (slurp in)))
        population (->> schema :types (map :name) (reduce (fn [m k] (assoc m k 1)) (sorted-map)))]
    (spit schema-path (json/generate-string schema {:pretty true}))
    (when population-path
      (spit population-path (json/generate-string population {:pretty true})))))

(defn load-config
  [schema]
  (config/load (json/parse-string (slurp schema) keyword)))

(defn generate
  [schema population out]
  (spit out (json/generate-string (world/gen (load-config schema)
                                             (json/parse-string (slurp population) keyword))
                                  {:pretty true})))

(defn -main [& [command & args]]
  (case command
    "bootstrap" (apply bootstrap args)
    "generate" (apply generate args)
    "help" (println "Usage: java -jar demo-data-standalone.jar command args*

Available commands:

  - help:      print this help

  - bootstrap IN SCHEMA [POPULATION]: create a schema from an swagger JSON spec
     - IN: path to swagger JSON specification file
     - SCHEMA: path to write a demo-data schema configuration file
     - POPULATION: (optional) path to write a demo-data population count map

  - generate SCHEMA POPULATION OUT:  generate a dataset
     - SCHEMA: path to schema configuration file
     - POPULATION: path to JSON population count map
     - OUT: path to write a JSON data set
")
    (println "Unknown command: try java -jar demo-data-standalone.jar help")))
