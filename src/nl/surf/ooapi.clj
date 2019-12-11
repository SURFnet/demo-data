(ns nl.surf.ooapi
  (:require [clojure.data.generators :as data.generators]
            [clojure.string :as string]
            [nl.surf.constraints :as constraints]
            [nl.surf.generators :as gen]
            [nl.surf.world :as world]))

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
  #{
    {:name :service/owner
     :deps [[:service/institution :institution/name]]}
    {:name      :service/institution
     :deps      [[:institution/institutionId]]
     :generator (world/pick-ref)}
    {:name      :service/logo
     :generator (constantly "https://example.com/logo.png")}
    ;; TODO Fill in rest of service attributes

    {:name      :institution/institutionId
     :generator (gen/uuid)}
    {:name      :institution/name
     :generator (gen/format "%s van %s"
                            (gen/one-of ["Universiteit" "Hogeschool" "Academie"])
                            (fn [{[city] :dep-vals}]
                              city))
     :deps      [[:institution/address-city]]}
    {:name      :institution/address
     :generator address-generator
     :deps      [[:institution/address-city]]}
    {:name      :institution/address-city
     :generator (-> "nl/city-names.txt" gen/lines-resource gen/one-of)}
    ;; TODO Fill in rest of institution attributes note: we don't need
    ;; links to educational-programmes, since they're all linked to
    ;; the institution by definition.

    {:name      :educational-programme/educationalProgrammeId
     :generator (gen/uuid)}
    {:name      :educational-programme/name
     :deps      [[:educational-programme/field-of-study]]
     :generator (fn [{{field :educational-programme/field-of-study} :entity :as world}]
                  ((gen/one-of (programme-names-by-field-of-study field)) world))}
    {:name      :educational-programme/field-of-study
     :generator (gen/one-of fields-of-study)}
    ;; TODO Fill in rest of educational programme

    {:name        :course/courseId
     :generator   (gen/int)
     :constraints [constraints/unique]}
    {:name      :course/name ;; TODO from list of names, depends on educational programme
     :generator (gen/string)}
    {:name      :course/abbreviation
     :deps      [[:course/name]]
     :generator (fn [{[name] :dep-vals}]
                  (abbreviate name))} ;; TODO should be unique
    {:name      :course/ects
     :generator (fn [_]
                  (* 2.5 (data.generators/geometric (/ 1.0 2.5))))}
    {:name      :course/educationalProgramme
     :deps      [[:educational-programme/educationalProgrammeId]]
     :generator (world/pick-ref)}
    ;; TODO Fill in rest of course


    {:name      :course-offering/courseOfferingId
     :generator (gen/uuid)}
    {:name      :course-offering/course
     :deps      [[:course/courseId]]
     :generator (world/pick-ref)}
    {:name      :course-offering/maxNumberStudents
     :generator (fn [_]
                  (+ 5 (data.generators/geometric (/ 1.0 20))))}

    ;; TODO Fill in rest of courseOffering

    ;; Lecturer links people to courseOfferings
    ;; people can only teach a courseOffering once
    {:name      :lecturer/refs
     :deps      [[:person/personId] [:course-offering/courseOfferingId]]
     :generator (world/pick-unique-refs)}
    {:name      :lecturer/person
     :deps      [[:lecturer/refs]]
     :generator (fn [{[[person _]] :dep-vals}]
                  person)}
    {:name      :lecturer/courseOffering
     :deps      [[:lecturer/refs]]
     :generator (fn [{[[_ courseOffering]] :dep-vals}]
                  courseOffering)}

    {:name      :person/personId
     :generator (gen/uuid)}
    {:name      :person/name
     :generator (gen/format "%s %s"
                            (-> "nl/first-names.txt" gen/lines-resource gen/one-of)
                            (-> "nl/last-names.txt" gen/lines-resource gen/one-of))}
    ;; TODO Fill in rest of person properties

    })




;;(world/gen attributes {:service 1 :institution 1, :educational-programme 3, :course 15, :lecturer 30, :course-offering 30 :person 30})
