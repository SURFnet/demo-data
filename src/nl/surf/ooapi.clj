(ns nl.surf.ooapi
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [nl.surf.constraints :as constraints]
            [nl.surf.date-util :as date-util]
            [nl.surf.export :as export]
            [nl.surf.generators :as gen]
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

(defn faculity-member? [affiliations]
  (seq (set/intersection #{"employee" "staff"} affiliations)))

(def bijbel-bla
  (-> "nl/bijbel.txt" gen/resource (gen/text :lines 10)))

(def attributes
  #{
    {:name :service/owner
     :deps [[:service/institution :institution/name]]}
    {:name      :service/logo
     :generator (constantly "https://example.com/logo.png")}
    {:name      :service/specification
     :generator (constantly "https://example.com/specification")}
    {:name      :service/documentation
     :generator (constantly "https://example.com/documentation")}
    {:name      :service/courseLevels
     :generator (constantly ["Bachelor" "Master"])}
    {:name      :service/roomTypes
     :generator (constantly ["General purpose", "Lecture hall" , "PC lab"])}
    {:name      :service/institution
     :deps      [[:institution/id]]
     :generator (world/pick-ref)}

    ;;;;;;;;;;;;;;;;;;;;

    {:name        :institution/id
     :generator   (gen/int)
     :constraints [constraints/unique]}
    {:name      :institution/brin
     :generator brin-generator}
    {:name      :institution/name
     :deps      [[:institution/addressCity]]
     :generator (gen/format "%s van %s"
                            (gen/one-of ["Universiteit" "Hogeschool" "Academie"])
                            (fn [{[city] :dep-vals}]
                              city))}
    {:name      :institution/domain
     :deps      [[:institution/name]]
     :generator (fn [{[name] :dep-vals :as world}]
                  (-> name s/lower-case s/trim (s/replace #"[^a-z0-9]+" "-") (str ".nl")))}
    {:name      :institution/description
     :generator bijbel-bla}
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

    {:name        :educational-programme/id
     :generator   (gen/int)
     :constraints [constraints/unique]}
    {:name      :educational-programme/name
     :deps      [[:educational-programme/fieldsOfStudy]]
     :generator (fn [{[field] :dep-vals :as world}]
                  ((gen/one-of (programme-names-by-field-of-study field)) world))}
    {:name      :educational-programme/description
     :generator bijbel-bla}
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
     :deps      [[:educational-programme/levelOfQualification]]
     :generator (fn [{[level] :dep-vals :as world}]
                  (if (= "Bachelor")
                    (* ((gen/int 2 8) world) 30)
                    (* ((gen/int-cubic 2 8) world) 30)))}
    {:name      :educational-programme/mainLanguage
     :generator (gen/weighted-set {"NL-nl" 5
                                   "GB-en" 1})}
    {:name      :educational-programme/qualificationAwarded
     :deps      [[:educational-programme/levelOfQualification] [:educational-programme/name]]
     :generator (fn [{[level name] :dep-vals}]
                  (format "%s of %s" level name))}
    {:name      :educational-programme/lengthOfProgramme
     :deps      [[:educational-programme/ects]]
     :generator (fn [{[ects] :dep-vals :as world}]
                  (-> ects (/ 60) (* 12) int))}
    {:name      :educational-programme/levelOfQualification
     :deps      [[:service/courseLevels]]
     :generator (fn [{{[service] :service} :world :as world}]
                  ((gen/one-of (:service/courseLevels service)) world))}
    {:name      :educational-programme/fieldsOfStudy
     :generator (gen/one-of fields-of-study)}
    {:name      :educational-programme/profileOfProgramme
     :generator bijbel-bla}
    {:name      :educational-programme/programmeLearningOutcomes
     :generator bijbel-bla}
    {:name      :educational-programme/modeOfStudy
     :generator (gen/weighted {"full-time"  5
                               "part-time"  2
                               "dual"       1
                               "e-learning" 2})}


    ;;;;;;;;;;;;;;;;;;;;

    {:name      :course-programme/refs
     :deps      [[:course/id]  [:educational-programme/id]]
     :generator (world/pick-unique-refs [true false])}
    {:name      :course-programme/course
     :deps      [[:course-programme/refs]]
     :generator (fn [{[[course]] :dep-vals}]
                  course)}
    {:name      :course-programme/educational-programme
     :deps      [[:course-programme/refs]]
     :generator (fn [{[[_ educational-programme]] :dep-vals}]
                  educational-programme)}

    ;;;;;;;;;;;;;;;;;;;;

    {:name        :course/id
     :generator   (gen/int)
     :constraints [constraints/unique]}
    {:name      :course/name
     ;; TODO: ensure that courses always have an educational-programme
     :deps      [[[:course-programme/course :course/id] :course-programme/educational-programme :educational-programme/fieldsOfStudy]]
     :generator (fn [{[fields] :dep-vals :as world}]
                  (let [g (gen/one-of-each (map programme-names-by-field-of-study fields))]
                    ((gen/format ((gen/one-of course-name-formats) world)
                                 (fn [_] (s/join " en " (or (seq (g world))
                                                            ["Tovenarij"])))) world)))}
    {:name        :course/abbreviation
     :deps        [[:course/name]]
     :generator   (fn [{[name] :dep-vals}]
                    (str (abbreviate name)
                         (when (> world/*retry-attempt-nr* 0) world/*retry-attempt-nr*)))
     :constraints [constraints/unique]}
    {:name      :course/ects
     :generator (fn [world]
                  (- 60 (* 2.5 ((gen/int-cubic 1 24) world))))}
    {:name      :course/description
     :generator bijbel-bla}
    {:name      :course/learningOutcomes
     :generator bijbel-bla}
    {:name      :course/goals
     :generator bijbel-bla}
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
     :deps      [[:service/courseLevels]]
     :generator (fn [{{[service] :service} :world :as world}]
                  ((gen/one-of (:service/courseLevels service)) world))}
    {:name      :course/format
     :generator (gen/weighted-set {"practicum"   1
                                   "hoorcollege" 1})}
    {:name      :course/modeOfDelivery
     :generator (gen/weighted-set {"e-learning"   1
                                   "face-to-face" 2
                                   "class-room"   20})}
    {:name      :course/mainLanguage
     :generator (gen/weighted-set {"NL-nl" 5
                                   "GB-en" 1})}
    {:name      :course/enrollment
     :generator bijbel-bla}
    {:name      :course/resources
     :generator bijbel-bla}
    {:name      :course/exams
     :generator bijbel-bla}
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
    {:name      :course/coordinator
     :deps      [[:person/id]]
     :generator (world/pick-ref)}

    ;;;;;;;;;;;;;;;;;;;;

    {:name        :course-offering/id
     :generator   (gen/int)
     :constraints [constraints/unique]}
    {:name      :course-offering/course
     :deps      [[:course/id]]
     :generator (world/pick-ref)}
    {:name      :course-offering/courseId
     :deps      [[:course-offering/course]]
     :generator (fn [{[[_ id]] :dep-vals}]
                  id)}
    {:name      :course-offering/maxNumberStudents
     :generator (gen/int-cubic 20 50)}
    {:name      :course-offering/currentNumberStudents
     :deps      [[:course-offering/maxNumberStudents]]
     :generator (fn [{[max] :dep-vals :as world}]
                  ((gen/int-cubic 10 max) world))}
    {:name      :course-offering/academicYear
     :generator (fn [world]
                  (let [year ((gen/int 1995 2020) world)]
                    (format "%d-%d" year (inc year))))}
    {:name      :course-offering/period
     :generator (gen/one-of ["1e periode"
                             "2e periode"
                             "3e periode"
                             "4e periode"])}

    ;;;;;;;;;;;;;;;;;;;;

    ;; Lecturer links people to courseOfferings, people can only teach a courseOffering once
    {:name      :lecturer/refs
     :deps      [[:person/id] [:course-offering/id]]
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

    {:name        :person/id
     :generator   (gen/int)
     :constraints [constraints/unique]}
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
    {:name      :person/dateOfBirth
     :deps      [[:person/affiliations]]
     :generator (fn [{[affiliations] :dep-vals :as world}]
                  (let [[min max] (if (faculity-member? affiliations)
                                    ["1950-01-01" "2000-01-01"]
                                    ["1990-01-01" "2005-01-01"])]
                    ((date-generator min max  gen/bigdec-cubic) world)))}
    {:name      :person/affiliations
     :generator (constantly #{"employee"})}
    {:name        :person/mail
     :deps        [[:institution/domain]
                   [:person/givenName] [:person/surnamePrefix] [:person/surname]]
     :generator   (fn [{{[institution] :institution} :world
                        [_ & name]                   :dep-vals}]
                    (str (-> (->> name (filter identity) (s/join " "))
                             s/trim
                             s/lower-case
                             (s/replace #"[^a-z0-9]+" "."))
                         (when (> world/*retry-attempt-nr* 0) world/*retry-attempt-nr*)
                         "@" (:institution/domain institution)))
     :constraints [constraints/unique]}
    {:name      :person/telephoneNumber
     :generator (gen/format "0%09d"
                            (gen/int 100000000 999999999))}
    {:name        :person/mobileNumber
     :generator   (gen/format "06%08d"
                              (gen/int 0 99999999))
     :constraints [constraints/unique]}
    {:name      :person/photoSocial
     :generator (constantly "https://docs.atlassian.com/aui/8.4.1/docs/images/avatar-person.svg")}
    {:name      :person/photoOfficial
     :generator (constantly "https://docs.atlassian.com/aui/8.4.1/docs/images/avatar-person.svg")}
    {:name      :person/gender
     :generator (gen/weighted {"M" 46
                               "F" 50
                               "X" 2
                               "U" 2})}
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
    {:name      :person/office
     :deps      [[:person/affiliations]]
     :generator (fn [{[affiliations] :dep-vals :as world}]
                  (when (faculity-member? affiliations)
                    ((gen/format "%c%c%c %c.%c%c"
                                 (gen/char \A \Z) (gen/char \A \Z) (gen/char \A \Z)
                                 (gen/char \0 \9) (gen/char \0 \9) (gen/char \0 \9))
                     world)))}})

(defn lecturers-for-offering
  [world course-offering-id]
  (keep (fn [{[_ id] :lecturer/courseOffering
              person :lecturer/person}]
          (when (= course-offering-id id)
            (world/get-entity world person)))
        (:lecturer world)))

(defn offerings-for-course
  [world course-id]
  (->> world
       :course-offering
       (filter #(= (second (:course-offering/course %)) course-id))))

(defn lecturers-for-course
  [world course-id]
  (->> course-id
       (offerings-for-course world)
       (map :course-offering/courseOfferingId)
       (mapcat #(lecturers-for-offering world %))
       set))

(defn programmes-for-course
  [world course-id]
  (keep (fn [{[_ id]    :course-programme/course
              programme :course-programme/educational-programme}]
          (when (= course-id id)
            (world/get-entity world programme)))
        (:course-programme world)))

(defn person-link
  [{:person/keys [displayName id]}]
  {:href  (str "/persons/" id)
   :title displayName})

(def export-conf
  {"/"                       {:type       :service
                              :singleton? true
                              :attributes {:service/institution {:hidden? true}}
                              :pre        (fn [e _]
                                            (assoc e :links {:_self {:href "/"}
                                                             :endpoints [{:href "/institution"}
                                                                         {:href "/educational-programmes"}
                                                                         {:href "/course-offerings"}
                                                                         {:href "/persons"}
                                                                         {:href "/courses"}]}))}
   "/institution"            {:type       :institution
                              :singleton? true
                              :attributes {:institution/address-city {:hidden? true}}
                              :pre        (fn [e _]
                                            (assoc e :links {:_self {:href "/institution"}
                                                             :educational-programmes {:href "/educational-programmes"}}))}
   "/educational-programmes" {:type :educational-programme
                              :pre  (fn [{:educational-programme/keys [id] :as e} _]
                                      (assoc e :links {:_self {:href (str "/educational-programmes/" id)}
                                                       :courses {:href (str "/courses?educationalProgramme=" id)}}))}
   "/course-offerings"       {:type       :course-offering
                              :attributes {:course-offering/course {:follow-ref? true
                                                                    :attributes  {:course/educationalProgramme {:hidden? true}}}}
                              :pre        (fn [{:course-offering/keys [id] :as e} world]
                                            (assoc e :links {:_self     {:href (str "/course-offerings/" id)}
                                                             :lecturers (mapv person-link
                                                                              (lecturers-for-offering world id))}))}
   "/persons"                {:type :person
                              :pre  (fn [{:person/keys [id] :as e} _]
                                      (assoc e :links {:_self {:href (str "/persons/" id)}}
                                        ; link to courses not
                                        ; implemented because that
                                        ; only supports students
                                             ))}
   "/courses"                {:type       :course
                              :attributes {:course/coordinator {:hidden? true}}
                              :pre        (fn [{:course/keys [id coordinator] :as e} world]
                                            (assoc e :links {:_self                 {:href (str "/courses/" id)}
                                                             :coordinator           (person-link (world/get-entity world coordinator))
                                                             :lecturers             (map person-link (lecturers-for-course world id))
                                                             :courseOfferings       {:href (str "/course-offerings?course=" id)}
                                                             :educationalProgrammes (map (fn [programme]
                                                                                           {:href (str "/educational-programmes/" (:educational-programme/id programme))})
                                                                                         (programmes-for-course world id))}))}})


;;(world/gen attributes {:service 1 :institution 1, :educational-programme 3, :course-programme 20 :course 15, :lecturer 30, :course-offering 30 :person 30})

;;(export/export (world/gen attributes {:service 1 :institution 1, :educational-programme 2, :course-programme 8 :course 5, :lecturer 20, :course-offering 10 :person 15}) export-conf)
