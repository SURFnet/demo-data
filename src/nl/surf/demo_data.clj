(ns nl.surf.demo-data
  (:require [nl.surf.demo-data.bootstrap :as bootstrap]
            [nl.surf.demo-data.world :as world]
            [clojure.java.io :as io]
            [cheshire.core :as json])
  (:gen-class))

(defn bootstrap
  [in out]
  (spit out (json/generate-string (bootstrap/spec->types (json/parse-string (slurp in))))))

(defn generate
  [schema population out]
  (spit out (json/generate-string (world/gen (json/parse-string (slurp schema) keyword)
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


