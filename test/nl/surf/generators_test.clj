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


