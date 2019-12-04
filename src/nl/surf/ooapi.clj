(ns nl.surf.ooapi
  (:require [clojure.data.generators :as data.generators]
            [clojure.string :as string]
            [nl.surf.constraints :as constraints]
            [nl.surf.generators :as gen]))

(def programme-names-by-field-of-study (-> "nl/programme-names.yml" gen/yaml-resource))
(def fields-of-study (keys programme-names-by-field-of-study))

(defn abbreviate
  [name]
  {:pre [(seq name)]}
  (->> (string/split name #"[^a-zA-Z]")
       (map first)
       (apply str)
       (string/upper-case)))

(def address-generator
  (gen/format "%s %d\n%d %c%c  %s"
              (-> "nl/street-names.txt" gen/lines-resource gen/one-of)
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
     :generator (-> "nl/city-names.txt" gen/lines-resource gen/one-of)}

    {:name :educational-programme/name
     :deps [:educational-programme/field-of-study]
     :generator (fn [{{field :educational-programme/field-of-study} :entity :as world}]
                  ((gen/one-of (programme-names-by-field-of-study field)) world))}
    {:name :educational-programme/field-of-study
     :generator (gen/one-of field-of-study)}

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
     :generator (gen/format "%s %s"
                            (-> "nl/first-names.txt" gen/lines-resource gen/one-of)
                            (-> "nl/last-names.txt" gen/lines-resource gen/one-of))}})





;;(world/gen attributes {:institution 1, :educational-programme 3, :course 15, :lecturer 30, :course-offering 30 :person 30})
