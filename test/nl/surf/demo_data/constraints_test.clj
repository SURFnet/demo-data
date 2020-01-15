(ns nl.surf.demo-data.constraints-test
  (:require  [clojure.test :refer [deftest is testing]]
             [nl.surf.demo-data.world :as world]
             [nl.surf.demo-data.generators :as generators]
             [nl.surf.demo-data.constraints :as constraints]))

(deftest test-constraints
  (testing "unique-constraint"
    (let [ids   #{1 2 3 4 5 6 7 8 9 10}
          world (world/gen #{{:name        :person/id
                              :generator   (generators/one-of ids)
                              :constraints [constraints/unique]}}
                           {:person 10})]
      (is (= 10 (count (:person world))))
      (is (every? ids (map :person/id (:person world))))
      (is (every? #{1} (vals (frequencies (map :person/id (:person world))))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unable to satisfy constraints"
                            (world/gen #{{:name        :person/id
                                          :generator   (generators/one-of ids)
                                          :constraints [constraints/unique]}}
                                       {:person 11}))))))
