(ns nl.surf.demo-data.bootstrap-test
  (:require [nl.surf.demo-data.bootstrap :as bootstrap]
            [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]))

(defonce spec (-> "ooapi.json"
                  io/resource
                  io/reader
                  json/parse-stream))

(defn schema-for
  [root]
  (get-in spec ["paths" root "get" "responses" "200"
                "content" "application/hal+json" "schema"]))

(deftest test-bootstrap
  (is (= {:name       "service"
          :attributes {"_____links__self__href" {:hidden true
                                                 :value "http://example.com/"},
                       "___links__endpoints"
                       {:hidden true
                        :value  [{"href" "/persons"} {"href" "/faculties"}
                                 {"href" "/educational-departments"}
                                 {"href" "/educational-plans"}
                                 {"href" "/educational-programmes"}
                                 {"href" "/institution"} {"href" "/course-groups"}
                                 {"href" "/courses"} {"href" "/course-offerings"}
                                 {"href" "/course-results"} {"href" "/test-results"}
                                 {"href" "/buildings"} {"href" "/rooms"}
                                 {"href" "/schedules"} {"href" "/news-feeds"}
                                 {"href" "/news-items"}]},
                       "___links__self"         {:hidden true
                                                 :deps      ["service/_____links__self__href"],
                                                 :generator ["object" "href"]},
                       "_links"                 {:deps      ["service/___links__self"
                                                             "service/___links__endpoints"],
                                                 :generator ["object" "self" "endpoints"]},
                       "courseLevels"           {:optional true, :value ["Bachelor" "Master"]},
                       "documentation"          {:value "http://example.com/"},
                       "logo"                   {:optional true, :value "http://example.com/"},
                       "owner"                  {:generator "string"},
                       "roomTypes"              {:optional true,
                                                 :value    ["General purpose" "Lecture hall" "PC lab"]},
                       "specification"          {:value "http://example.com/"}}}
         (bootstrap/schema->type "service" (schema-for "/"))))
  (is (= {:name       "institution"
          :attributes {"_____links__educational-programmes__href" {:hidden true
                                                                   :value "http://example.com/"},
                       "_____links__self__href"                   {:hidden true
                                                                   :value "http://example.com/"},
                       "___links__educational-programmes"         {:hidden true
                                                                   :deps      ["institution/_____links__educational-programmes__href"],
                                                                   :generator ["object" "href"]},
                       "___links__self"                           {:hidden true
                                                                   :deps      ["institution/_____links__self__href"],
                                                                   :generator ["object" "href"]},
                       "__academicCalendar__calender"             {:hidden true
                                                                   :generator "string"},
                       "__academicCalendar__year"                 {:hidden true
                                                                   :generator "string"},
                       "__address__additional"                    {:hidden true
                                                                   :generator "string"},
                       "__address__city"                          {:hidden true
                                                                   :generator "string"},
                       "__address__countryCode"                   {:hidden true
                                                                   :generator "string"},
                       "__address__street"                        {:hidden true
                                                                   :generator "string"},
                       "__address__zip"                           {:hidden true
                                                                   :generator "string"},
                       "_links"                                   {:deps      ["institution/___links__self"
                                                                               "institution/___links__educational-programmes"],
                                                                   :generator ["object" "self" "educational-programmes"]},
                       "academicCalendar"
                       {:deps      ["institution/__academicCalendar__year"
                                    "institution/__academicCalendar__calender"],
                        :generator ["object" "year" "calender"],
                        :optional  true},
                       "address"                                  {:deps      ["institution/__address__street"
                                                                               "institution/__address__additional"
                                                                               "institution/__address__city"
                                                                               "institution/__address__zip"
                                                                               "institution/__address__countryCode"],
                                                                   :generator ["object" "street" "additional" "city" "zip"
                                                                               "countryCode"],
                                                                   :optional  true},
                       "brin"                                     {:generator "string", :optional true},
                       "description"                              {:generator "string"},
                       "institutionId"                            {:generator "string", :optional true},
                       "logo"                                     {:optional true, :value "http://example.com/"},
                       "name"                                     {:generator "string"}}}
         (bootstrap/schema->type "institution" (schema-for "/institution")))))
