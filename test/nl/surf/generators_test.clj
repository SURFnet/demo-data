(ns nl.surf.generators-test
  (:require [nl.surf.generators :as generators]
            [clojure.test :refer [deftest testing is]]))

(deftest test-generators
  (testing "int-generator"
    (is (every? int? (repeatedly 10 #(generators/int {})))))
  (testing "one-of-generator"
    (let [values #{1 2 3}]
      (is (every? values (repeatedly 10 #((generators/one-of values) {})))))))
