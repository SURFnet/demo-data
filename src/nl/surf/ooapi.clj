(ns nl.surf.ooapi
  (:require [nl.surf.generators :as gen]
            [clojure.data.generators :as data.generators]
            [clojure.string :as string]))

(defn abbreviate
  [name]
  (->> (string/split name #"[^a-zA-Z]")
       (map first)
       (apply str)
       (string/upper-case)))

(def attributes
  #{{:name      :course/courseId
     :generator gen/uuid}
    {:name      :course/name ;; TODO from list of names, depends on educational programme
     :generator gen/string}
    {:name      :course/abbreviation
     :deps      [:course/name]
     :generator (fn [{{name :course/name} :entity}]
                  (abbreviate name))} ;; TODO should be unique
    {:ects :course/ects
     :generator (fn [_]
                  (* 2.5 (data.generators/geometric (/ 1.0 2.5))))}})

