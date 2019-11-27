(ns nl.surf.generators-test
  (:require [nl.surf.generators :as generators]
            [nl.surf.world :as world]
            [clojure.test :refer [deftest testing is]]))

(deftest test-generators
  (testing "int-generator"
    (let [world (world/gen #{{:name      :person/id
                              :generator generators/int}}
                           {:person 10})]
      (is (= 10 (count (:person world))))
      (is (every? int? (map :person/id (:person world))))))
  (testing "one-of-generator"
    (let [world (world/gen #{{:name      :person/id
                              :generator (generators/one-of #{1 2 3})}}
                           {:person 10})]
      (is (= 10 (count (:person world))))
      (is (every? #{1 2 3} (map :person/id (:person world)))))))

(deftest test-constraints
  (testing "unique-constraint"
    (let [ids   #{1 2 3 4 5 6 7 8 9 10}
          world (world/gen #{{:name        :person/id
                              :generator   (generators/one-of ids)
                              :constraints [generators/unique]}}
                           {:person 10})]
      (is (= 10 (count (:person world))))
      (is (every? ids (map :person/id (:person world))))
      (is (every? #{1} (vals (frequencies (map :person/id (:person world))))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unable to satisfy constraints"
                            (world/gen #{{:name        :person/id
                                          :generator   (generators/one-of ids)
                                          :constraints [generators/unique]}}
                                       {:person 11}))))))
