(ns nl.surf.world-test
  (:require [clojure.test :refer [are deftest is]]
            [nl.surf.generators :as gen]
            [nl.surf.world :as sut]))

(deftest sort-attrs
  (are [res attrs] (= res (sut/sort-attrs attrs))
    [] #{}

    [{:name :pig/name} {:name :dog/name :deps [:pig/name]} {:name :cat/name :deps [:dog/name]}]
    #{{:name :cat/name :deps [:dog/name]} {:name :dog/name :deps [:pig/name]} {:name :pig/name}}))

(deftest gen
  (let [attrs  #{{:name      :cat/id
                  :generator gen/gen-uniq-id}
                 {:name      :cat/name
                  :generator (fn [state]
                               (str (-> (sut/get-relation state :person)
                                        :person/name) "'s cat"))
                  :deps      [:person/name]}
                 {:name :cat/owner-id
                  :deps [:person/id]}
                 {:name      :person/id
                  :generator gen/gen-uniq-id}
                 {:name      :person/name
                  :generator gen/gen-name}}
        dist   {:cat    4
                :person 3}
        result (sut/gen attrs dist)]
    (is (= 4 (-> result :cat count)))
    (is (= 3 (-> result :person count)))))
