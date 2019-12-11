(ns nl.surf.ooapi
  (:require [clojure.data.generators :as data.generators]
            [clojure.string :as s]
            [nl.surf.constraints :as constraints]
            [nl.surf.generators :as gen]
            [nl.surf.date-util :as date-util]
            [nl.surf.export :as export]
            [nl.surf.world :as world]))


(def programme-names-by-field-of-study (-> "nl/programme-names.yml" gen/yaml-resource))
(def fields-of-study (keys programme-names-by-field-of-study))
(def course-name-formats ["Inleiding tot %s"
                          "Geschiedenis van de %s"
                          "Filosofie van %s"
                          "Psychologie van de %s"
                          "Wiskunde van de %s"
                          "Macro %s"
                          "Micro %s"
                          "%s in de praktijk"
                          "%s in de vorige eeuw"
                          "%s van de toekomst"
                          "%s voor gevorderden"])

(defn abbreviate
  [name]
  {:pre [(seq name)]}
  (->> (s/split name #"[^a-zA-Z]")
       (map first)
       (apply str)
       (s/upper-case)))

(defn date-generator
  [^String lo, ^String hi, gen]
  (fn [world]
    (let [lo (date-util/->msecs-since-epoch (date-util/parse-date lo))
          hi (date-util/->msecs-since-epoch (date-util/parse-date hi))]
      (date-util/<-msecs-since-epoch ((gen lo hi) world)))))

(def brin-generator
  (gen/format "%c%c%c%c"
              (gen/char \0 \9)
              (gen/char \0 \9)
              (gen/char \A \Z)
              (gen/char \A \Z)))

(def lorum-ipsum
  (-> "lorum-ipsum.txt" gen/resource (gen/text :lines 10)))

(def attributes
  #{
    {:name :service/owner
     :deps [[:service/institution :institution/name]]}
    {:name      :service/logo
     :generator (constantly "https://example.com/logo.png")}
    {:name      :service/specification
     :generator (constantly "TODO")}
    {:name      :service/documentation
     :generator (constantly "TODO")}
    {:name      :service/courseLevels
     :generator (constantly "TODO")}
    {:name      :service/roomTypes
     :generator (constantly "TODO")}
    {:name      :service/institution
     :deps      [[:institution/institutionId]]
     :generator (world/pick-ref)}

    ;;;;;;;;;;;;;;;;;;;;

    {:name      :institution/institutionId
     :generator (gen/uuid)}
    {:name      :institution/brin
     :generator brin-generator}
    {:name      :institution/name
     :generator (gen/format "%s van %s"
                            (gen/one-of ["Universiteit" "Hogeschool" "Academie"])
                            (fn [{[city] :dep-vals}]
                              city))
     :deps      [[:institution/addressCity]]}
    {:name      :institution/description
     :generator lorum-ipsum}
    {:name      :institution/academicCalendar
     :generator (constantly "https://to.some/random/location")}
    {:name      :institution/address
     :generator (gen/format "%s %d\n%d %c%c  %s"
                            (-> "nl/street-names.txt" gen/lines-resource gen/one-of)
                            (gen/int 1 200)
                            (gen/int 1011 9999)
                            (gen/char \A \Z)
                            (gen/char \A \Z)
                            (fn [{{city :institution/addressCity} :entity}] city))
     :deps      [[:institution/addressCity]]}
    {:name      :institution/addressCity
     :generator (-> "nl/city-names.txt" gen/lines-resource gen/one-of)}
    {:name      :institution/logo
     :generator (constantly "https://to.some/random/location")}

    {:name      :educational-programme/educationalProgrammeId
     :generator (gen/uuid)}
    {:name      :educational-programme/name
     :deps      [[:educational-programme/fieldsOfStudy]]
     :generator (fn [{[field] :dep-vals :as world}]
                  ((gen/one-of (programme-names-by-field-of-study field)) world))}
    {:name      :educational-programme/description
     :generator lorum-ipsum}
    {:name      :educational-programme/termStartDate
     :generator (fn [world]
                  (date-util/nth-weekday-of 0 date-util/monday
                                            ((gen/int 1990 2018) world)
                                            ((gen/one-of [date-util/september date-util/february]) world)))}
    {:name      :educational-programme/termEndDate
     :generator (fn [{[start-date] :dep-vals :as world}]
                  (let [max-year   2018
                        start-year (inc (date-util/get start-date date-util/year))]
                    (when (and (< start-year max-year)
                               (= 0 ((gen/int 0 4) world)))
                      (let [year ((gen/int start-year max-year) world)]
                        (date-util/last-day-of year ((gen/one-of [date-util/august date-util/january]) world))))))
     :deps      [[:educational-programme/termStartDate]]}
    {:name      :educational-programme/ects
     :generator (fn [world]
                  (* ((gen/int 2 8) world) 30))}
    {:name      :educational-programme/mainLanguage
     :generator (gen/one-of ["NL-nl" "EN-gb"])}
    {:name      :educational-programme/qualificationAwarded
     :generator (constantly "TODO")}
    {:name      :educational-programme/lengthOfProgramme
     :deps      [[:educational-programme/ects]]
     :generator (fn [{[ects] :dep-vals :as world}]
                  (-> ects (/ 60) (* 12) int))} ;; TODO how many months in a year?
    {:name      :educational-programme/levelOfQualification
     :generator (constantly "TODO")}
    {:name      :educational-programme/fieldsOfStudy
     :generator (gen/one-of fields-of-study)}
    {:name      :educational-programme/profileOfProgramme
     :generator lorum-ipsum}
    {:name      :educational-programme/programmeLearningOutcomes
     :generator lorum-ipsum}
    {:name      :educational-programme/modeOfStudy
     :generator (gen/weighted {"full-time"  5
                               "part-time"  2
                               "dual"       1
                               "e-learning" 2})}

    ;;;;;;;;;;;;;;;;;;;;

    {:name        :course/courseId
     :generator   (gen/int)
     :constraints [constraints/unique]}
    {:name      :course/name
     :deps      [[:course/educationalProgramme :educational-programme/fieldsOfStudy]]
     :generator (fn [{[field] :dep-vals :as world}]
                  ((gen/format ((gen/one-of course-name-formats) world)
                               (gen/one-of (get programme-names-by-field-of-study field))) world))}
    {:name      :course/abbreviation
     :deps      [[:course/name]]
     :generator (fn [{[name] :dep-vals}]
                  (abbreviate name))} ;; TODO should be unique
    {:name      :course/ects
     :generator (fn [_]
                  (* 2.5 (data.generators/geometric (/ 1.0 2.5))))}
    {:name      :course/description
     :generator lorum-ipsum}
    {:name      :course/learningOutcomes
     :generator lorum-ipsum}
    {:name      :course/goals
     :generator lorum-ipsum}
    {:name      :course/requirements
     :deps      [[:course/name]]
     :generator (fn [{{:keys [course]} :world
                      [name]           :dep-vals
                      :as              world}]
                  (when (= 0 ((gen/int 0 2) world))
                    ((gen/one-of (filter (fn [v] (not= name (:course/name v)))
                                         (map :course/name course)))
                     world)))}
    {:name      :course/level
     :generator (constantly "TODO")}
    {:name      :course/format
     :generator (constantly "TODO")}
    {:name      :course/modeOfDelivery
     :generator (constantly "TODO")}
    {:name      :course/mainLanguage
     :generator (constantly "TODO")}
    {:name      :course/enrollment
     :generator lorum-ipsum}
    {:name      :course/resources
     :generator lorum-ipsum}
    {:name      :course/exams
     :generator lorum-ipsum}
    {:name      :course/schedule
     :generator (gen/weighted {"1e periode" 2
                               "2e periode" 2
                               "3e periode" 2
                               "4e periode" 2
                               "jan-feb"    1
                               "feb-mrt"    1
                               "mrt-apr"    1
                               "apr-mei"    1
                               "mei-jun"    1
                               "jun-jul"    1
                               "sep-okt"    1
                               "okt-nov"    1
                               "nov-dec"    1})}
    {:name      :course/educationalProgramme
     :deps      [[:educational-programme/educationalProgrammeId]]
     :generator (world/pick-ref)}

    ;;;;;;;;;;;;;;;;;;;;

    {:name      :course-offering/courseOfferingId
     :generator (gen/uuid)}
    {:name      :course-offering/course
     :deps      [[:course/courseId]]
     :generator (world/pick-ref)}
    {:name      :course-offering/maxNumberStudents
     :generator (fn [_]
                  (+ 5 (data.generators/geometric (/ 1.0 20))))}
    {:name      :course-offering/currentNumberStudents
     :generator (constantly "TODO")}
    {:name      :course-offering/academicYear
     :generator (constantly "TODO")}
    {:name      :course-offering/period
     :generator (constantly "TODO")}

    ;;;;;;;;;;;;;;;;;;;;

    ;; Lecturer links people to courseOfferings, people can only teach a courseOffering once
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

    ;;;;;;;;;;;;;;;;;;;;

    {:name      :person/personId
     :generator (gen/uuid)}
    {:name      :person/givenName
     :generator (-> "nl/first-names.txt" gen/lines-resource gen/one-of)}
    {:name      :person/surname
     :generator (-> "nl/last-names.txt" gen/lines-resource gen/one-of)}
    {:name      :person/surnamePrefix
     :generator (gen/weighted {nil       50
                               "van"     3
                               "van de"  3
                               "van het" 3
                               "van 't"  2
                               "in de"   2
                               "in 't"   2
                               "aan de"  1
                               "aan het" 1
                               "bij"     1
                               "bij de"  1
                               "bij het" 1
                               "op de"   1
                               "op het"  1
                               "op 't"   1})}
    {:name      :person/displayName
     :deps      [[:person/title] [:person/givenName] [:person/surnamePrefix] [:person/surname]]
     :generator (fn [{:keys [dep-vals]}]
                  (->> dep-vals (filter identity) (s/join " ")))}
#_    {:name      :person/dateOfBirth
     :generator (date-generator "1980-01-01" "2005-01-01" gen/int-log)}

    {:name      :person/title
     :generator (gen/weighted {nil     50
                               "dr."   4
                               "mr."   4
                               "ir."   4
                               "ing."  5
                               "drs."  4
                               "prof." 4
                               "bacc." 2
                               "kand." 2})}


    }  )

(defn lecturers-for-offering
  [world course-offering-id]
  (keep (fn [{[_ courseOfferingId] :lecturer/courseOffering
              person               :lecturer/person}]
          (when (= course-offering-id courseOfferingId)
            (world/get-entity world person)))
        (:lecturer world)))


(def export-conf
  {"/"                       {:type       :service
                              :singleton? true
                              :attributes {:service/institution {:hidden? true}}
                              :pre        (fn [e _]
                                            (assoc e :links [{:_self {:href "/"}}
                                                             :endpoints [{:href "/institution"}
                                                                         {:href "/educational-programmes"}
                                                                         {:href "/course-offerings"}
                                                                         {:href "/persons"}
                                                                         {:href "/courses"}]]))}
   "/institution"            {:type       :institution
                              :singleton? true
                              :attributes {:institution/address-city {:hidden? true}}
                              :pre        (fn [e _]
                                            (assoc e :links [{:_self {:href "/institution"}}
                                                             {:educational-programmes {:href "/educational-programmes"}}]))}
   "/educational-programmes" {:type :educational-programme
                              :pre  (fn [{:educational-programme/keys [educationalProgrammeId] :as e} _]
                                      (assoc e :links [{:_self {:href (str "/educational-programmes/" educationalProgrammeId)}}
                                                       {:courses {:href (str "/courses?educationalProgramme=" educationalProgrammeId)}}]))}
   "/course-offerings"       {:type       :course-offering
                              :attributes {:course-offering/course {:follow-ref? true
                                                                    :attributes  {:course/educationalProgramme {:hidden? true}}}}
                              :pre        (fn [{:course-offering/keys [courseOfferingId] :as e} world]
                                            (assoc e :links [{:_self     {:href (str "/course-offerings/" courseOfferingId)}
                                                              :lecturers (mapv (fn [{:person/keys [displayName personId]}]
                                                                                 {:href  (str "/persons/" personId)
                                                                                  :title displayName})
                                                                               (lecturers-for-offering world courseOfferingId))}]))}
   "/persons"                {:type :person
                              :pre  (fn [{:person/keys [personId] :as e} _]
                                      (assoc e :links [{:_self {:href (str "/persons/" personId)}}
                                        ; link to courses not
                                        ; implemented because that
                                        ; only supports students
                                                       ]))}
   "/courses"                {:type       :course
                              :attributes {:course/educationalProgramme {:hidden? true}}}})


;;(world/gen attributes {:service 1 :institution 1, :educational-programme 3, :course 15, :lecturer 30, :course-offering 30 :person 30})

;;(export/export (world/gen attributes {:service 1 :institution 1, :educational-programme 2, :course 5, :lecturer 20, :course-offering 10 :person 15}) export-conf)
