(ns nl.surf.ooapi
  (:require [nl.surf.generators :as gen]
            [nl.surf.world :as world]
            [nl.surf.constraints :as constraints]
            [clojure.data.generators :as data.generators]
            [clojure.string :as string]))

(defn abbreviate
  [name]
  {:pre [(seq name)]}
  (->> (string/split name #"[^a-zA-Z]")
       (map first)
       (apply str)
       (string/upper-case)))

(def address-generator
  (gen/format "%s %d\n%d %c%c  %s"
              (-> "nl/street-names.txt" gen/split-resource-lines gen/one-of)
              (gen/int 1 200)
              (gen/int 1011 9999)
              (gen/char \A \Z)
              (gen/char \A \Z)
              (fn [{{city :institution/address-city} :entity}] city)))

(def attributes
  #{{:name      :institution/name
     :generator (gen/format "%s van %s"
                            (gen/one-of ["Universiteit" "Hogeschool" "Academie"])
                            (fn [{{city :institution/address-city} :entity}] city))
     :deps      [:institution/address-city]}
    {:name      :institution/address
     :generator address-generator
     :deps      [:institution/address-city]}
    {:name      :institution/address-city
     :generator (-> "nl/city-names.txt" gen/split-resource-lines gen/one-of)}

    {:name        :course/courseId
     :generator   (gen/int)
     :constraints [constraints/unique]}
    {:name      :course/name ;; TODO from list of names, depends on educational programme
     :generator (gen/string)}
    {:name      :course/abbreviation
     :deps      [:course/name]
     :generator (fn [{{name :course/name} :entity}]
                  (abbreviate name))} ;; TODO should be unique
    {:name      :course/ects
     :generator (fn [_]
                  (* 2.5 (data.generators/geometric (/ 1.0 2.5))))}
    {:name      :course-offering/courseOfferingId
     :generator (gen/uuid)}
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
     :generator (gen/uuid)}
    {:name      :person/name
     :generator (fn [_]
                  (data.generators/one-of "Bubbles" "Percy" "Mary" "Fred" "Wilma"))}})





;;(world/gen attributes {:institution 1, :course 3, :lecturer 2, :course-offering, 3 :person 10})
