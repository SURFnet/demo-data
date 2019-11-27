(ns nl.surf.world-test
  (:require [clojure.test :refer [are deftest is]]
            [nl.surf.generators :as gen]
            [nl.surf.world :as sut]))

(deftest sort-attrs
  (are [res attrs] (= res (sut/sort-attrs attrs))
    [] []

    [{:name :a}
     {:name :b :deps [:a]}
     {:name :c :deps [:b]}
     {:name :d :deps [:c]}]
    [{:name :c :deps [:b]}
     {:name :b :deps [:a]}
     {:name :a}
     {:name :d :deps [:c]}]

    [{:name :a}
     {:name :b}
     {:name :d :deps [:a]}
     {:name :c :deps [:a :b]}
     {:name :e :deps [:b]}]
    [{:name :c :deps [:a :b]}
     {:name :a}
     {:name :d :deps [:a]}
     {:name :e :deps [:b]}
     {:name :b}])

  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"circular dependency detected"
                        (sut/sort-attrs #{{:name :a :deps [:b]}
                                          {:name :b :deps [:a]}}))))

(deftest gen
  (let [attrs  #{{:name      :cat/id
                  :generator gen/uuid}
                 {:name      :cat/name
                  :generator (fn [state]
                               (str (-> (sut/get-relation state :person)
                                        :person/name) "'s cat"))
                  :deps      [:person/name]}
                 {:name :cat/owner-id
                  :deps [:person/id]}
                 {:name      :person/id
                  :generator gen/uuid}
                 {:name      :person/name
                  :generator gen/uuid}}
        dist   {:cat    4
                :person 3}
        result (sut/gen attrs dist)]
    (is (= 4 (-> result :cat count)))
    (is (= 3 (-> result :person count)))))

(deftest test-compose-constraints
  (let [constraint (#'nl.surf.world/compose-constraints [(fn [_ v]
                                                           (pos? v))
                                                         (fn [_ v]
                                                           (even? v))])]
    (are [b v] (= b (constraint {} v))
      false 33
      true  34
      false 0
      false -2)))
