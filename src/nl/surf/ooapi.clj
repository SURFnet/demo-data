(ns nl.surf.ooapi
  (:require [nl.surf.generators :as gen]
            [nl.surf.world :as world]
            [clojure.data.generators :as data.generators]
            [clojure.string :as string]))

(defn abbreviate
  [name]
  {:pre [(seq name)]}
  (->> (string/split name #"[^a-zA-Z]")
       (map first)
       (apply str)
       (string/upper-case)))

(def attributes
  #{{:name      :course/courseId
     :generator gen/int
     :constraints [gen/unique]}
    {:name      :course/name ;; TODO from list of names, depends on educational programme
     :generator gen/string}
    {:name      :course/abbreviation
     :deps      [:course/name]
     :generator (fn [{{name :course/name} :entity}]
                  (abbreviate name))} ;; TODO should be unique
    {:name      :course/ects
     :generator (fn [_]
                  (* 2.5 (data.generators/geometric (/ 1.0 2.5))))}
    {:name      :course-offering/courseOfferingId
     :generator gen/uuid}
    {:name :course-offering/courseId
     :deps [:course/courseId]}
    {:name      :course-offering/maxNumberStudents
     :generator (fn [_]
                  (+ 5 (data.generators/geometric (/ 1.0 20))))}
    {:name :lecturer/personId
     :deps [:person/personId]}
    {:name :lecturer/courseOfferingId
     :deps [:course-offering/courseOfferingId]}
    {:name      :person/personId
     :generator gen/uuid}
    {:name      :person/name
     :generator (fn [_]
                  (data.generators/one-of "Bubbles" "Percy" "Mary" "Fred" "Wilma"))}})





;;(world/gen attributes {:course 3 :lecturer 2 :course-offering 3 :person 10})
