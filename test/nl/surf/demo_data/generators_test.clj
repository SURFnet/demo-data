(ns nl.surf.demo-data.generators-test
  (:require [clojure.test :refer [deftest is testing]]
            [nl.surf.demo-data.generators :as sut]))

(deftest generators
  (testing "int"
    (is (every? int? (repeatedly 10 #((sut/int) {})))))
  (testing "one-of"
    (let [values #{1 2 3}]
      (is (every? values (repeatedly 10 #((sut/one-of values) {}))))))
  (testing "format"
    (is (= "Fred and Wilma Flintstone"
           ((sut/format "%s and %s %s"
                        (sut/one-of ["Fred"])
                        (sut/one-of ["Wilma"])
                        (sut/one-of ["Flintstone"])) {}))))
  (testing "object"
    (is (= {:a "A"
            :b "B"
            :c "C"}
           ((sut/object {:a (sut/one-of ["A"])
                         :b (sut/one-of ["B"])
                         :c (sut/one-of ["C"])}) {})))))
