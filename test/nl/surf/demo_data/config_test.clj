(ns nl.surf.demo-data.config-test
  (:refer-clojure :exclude [load])
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [nl.surf.demo-data.config :as sut]))

(deftest load
  (testing "attributes"
    (testing "basic"
      (let [attr (-> {:types [{:name       "this"
                               :attributes {:att {:generator ["constantly" "test"]}}}]}
                     sut/load
                     first)]
        (is (= :this/att (:name attr)))
        (is (-> attr :generator fn?))
        (is (= "constantly" (-> attr :generator meta :name)))
        (is (= ["test"] (-> attr :generator meta :arguments)))
        (is (= "test" ((:generator attr) {})))))

    (testing "pass through attribute props"
      (let [attr (-> {:types [{:name       "this"
                               :attributes {:att {:optional true, :testing 1}}}]}
                     sut/load
                     first)]
        (is (= true (:optional attr)))
        (is (= 1 (:testing attr)))))

    (testing "deps"
      (let [attr (-> {:types [{:name       "this"
                               :attributes {:att {:generator "constantly"
                                                  :deps      ["other/attr"]}}}]}
                     sut/load
                     first)]
        (is (= :this/att (:name attr)))
        (is (fn? (:generator attr)))
        (is (= "test" ((:generator attr) {:dep-vals ["test"]})))))

    (testing "value"
      (let [attr (-> {:types [{:name       "this"
                               :attributes {:att {:value "test"}}}]}
                     sut/load
                     first)]
        (is (= :this/att (:name attr)))
        (is (fn? (:generator attr)))
        (is (= "test" ((:generator attr) {:dep-vals ["test"]}))))))

  (testing "refs"
    (testing "basic"
      (let [attrs (-> {:types [{:name "this"
                                :refs {:other {:deps ["that/id"]}}}]}
                      sut/load)]
        (is (= 1 (count attrs)))
        (is (= :this/other (-> attrs first :name)))
        (is (fn? (-> attrs first :generator)))
        (is (= "ref" (-> attrs first :generator meta :name)))))

    (testing "unique"
      (let [attrs (-> {:types [{:name "this"
                                :refs {:that-other {:unique     true
                                                    :deps       ["that/id" "other/id"]
                                                    :attributes ["that" "other"]}}}]}
                      sut/load)]
        (is (= 3 (count attrs)))
        (is (= #{:this/that-other :this/that :this/other} (set (map :name attrs))))
        (is (fn? (-> attrs first :generator)))
        (is (= #{"unique-refs" "that-other/other" "that-other/that"}
               (->> attrs (map :generator) (map meta) (map :name) set)))))))

(deftest load-json
  (testing "generator with object argument"
    (let [attr (-> {:types [{:name       "this"
                             :attributes {:att {:generator ["weighted" {"foo" 1, "bar" 2}]}}}]}
                   json/generate-string
                   sut/load-json
                   first)]
      (is (= :this/att (:name attr)))
      (is (-> attr :generator fn?))
      (is (= "weighted" (-> attr :generator meta :name)))
      (is (= [{"foo" 1, "bar" 2}] (-> attr :generator meta :arguments))))))
