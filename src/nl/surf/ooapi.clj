(ns nl.surf.ooapi
  (:require [nl.surf.prop :as prop]
            [clojure.data.generators :as gen]))

;;;; Generators signature is
;;;; (fn [{:keys [entities path]]) => value


(def cities
  ["Amersfoort" "Harderwijk" "Gorinchem" "Broek op Waterland"])

(defn institution-name
  [_]
  (str (gen/one-of "Universiteit" "Hogeschool")
       " van "
       (apply gen/one-of cities)))

(def random-id
  (constrain prop/pos-int
             (fn [{:keys [entities path]} val]
               (not (get-in entities [(first path) val])))))

(def generators
  {:service               {:owner         institution-name
                           :logo          (constantly "http://example.org/logo.png")
                           :specification (constantly "http://example.org/")
                           :documentation (constantly "https://rawgit.com/open-education-api/specification/v3/docs.html")
                           :courseLevels  (prop/subset #{"Bachelor" "Master"})
                           :roomTypes     (constantly ["General purpose", "Lecture hall", "PC lab"])
                           :_links        (constantly {:self      {:href "/service"}
                                                       :endpoints [{:href "/persons"}
                                                                   {:href "/educational-programmes"}
                                                                   {:href "/courses"}
                                                                   {:href "/course-offerings"}
                                                                   {:href "/institutions"}]})}
   :institution           {:institutionId `random-id
                           :brin          #"\d{2}[A-Z]{2}"
                           :name `institution-name}
   :educational-programme {:educationalProgrammeId `random-id
                           :name                   `educational-programme-name
                           :description            `lorem-ipsum
                           :termStartDate          `rfc-3339
                           :termEndDate            `(and rfc-339 (> :termStartDate))
                           :ects                   `(and int? (>= 0))
                           :mainLanguage           #{"nl-NL" "en-US"}
                                        ; ....
                           }
   })
