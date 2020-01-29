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
  (:require [nl.surf.demo-data.bootstrap :as bootstrap]
            [nl.surf.demo-data.config :as config]
            [nl.surf.demo-data.world :as world]
            [clojure.java.io :as io]
            [cheshire.core :as json])
  (:gen-class))

(defn bootstrap
  [in out]
  (spit out (json/generate-string (bootstrap/spec->types (json/parse-string (slurp in))))))

(defn load-config
  [schema]
  (config/load (json/parse-string (slurp schema) keyword)))

(defn generate
  [schema population out]
  (spit out (json/generate-string (world/gen (load-config schema)
                                             (json/parse-string (slurp population) keyword)))))

(defn -main [& [command & args]]
  (case command
    "bootstrap" (apply bootstrap args)
    "generate" (apply generate args)
    "help" (println "Usage: java -jar demo-data-standalone.jar command args*

Available commands:

  - help:      print this help

  - bootstrap $in $out: create a schema from an openapi json spec
     - $in: path to openapi json specification file
     - $out: path to write a demo-data schema configuration file

  - generate $schema $population $out:  generate a dataset
     - $schema: path to schema configuration file
     - $population: path to json population count map
     - $out: path to write a json data set
")
    (println "Unknown command: try java -jar demo-data-standalone.jar help")))


