(ns nl.surf.world-test
  (:require [clojure.test :refer [are deftest is]]
            [clojure.string :as string]
            [nl.surf.generators :as gen]
            [nl.surf.world :as sut]
            [nl.surf.constraints :as constraints]))

(deftest sort-attrs
  (are [res attrs] (= res (sut/sort-attrs attrs))
    [] []

    [{:name :a}
     {:name :b :deps [[:a]]}
     {:name :c :deps [[:b]]}
     {:name :d :deps [[:c]]}]
    [{:name :c :deps [[:b]]}
     {:name :b :deps [[:a]]}
     {:name :a}
     {:name :d :deps [[:c]]}]

    [{:name :a}
     {:name :b}
     {:name :d :deps [[:a]]}
     {:name :c :deps [[:a] [:b]]}
     {:name :e :deps [[:b]]}]
    [{:name :c :deps [[:a] [:b]]}
     {:name :a}
     {:name :d :deps [[:a]]}
     {:name :e :deps [[:b]]}
     {:name :b}])

  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"circular dependency detected"
                        (sut/sort-attrs #{{:name :a :deps [[:b]]}
                                          {:name :b :deps [[:a]]}}))
      "detect circular dependencies")

  (is (thrown-with-msg? clojure.lang.ExceptionInfo #".*dependency on undefined attribute.*"
                        (sut/sort-attrs #{{:name :a :deps [[:b]]}}))
      "detect dependencies on non-existing attributes"))

(deftest test-get-entity
  (is (= {:foo/id 2 :foo/name "Fred"}
         (sut/get-entity {:foo [{:foo/id 1} {:foo/id 2 :foo/name "Fred"}]}
                         [:foo/id 2]))))

(deftest gen
  (let [attrs  #{{:name      :cat/id
                  :generator (gen/uuid)}
                 {:name      :cat/owner
                  :deps      [[:person/id]]
                  :generator (sut/pick-ref)}
                 {:name      :cat/name
                  :generator (fn [{[owner-name] :dep-vals}]
                               (str owner-name "'s cat"))
                  :deps      [[:cat/owner :person/name]]}
                 {:name :cat/owner-id
                  :deps [[:person/id]]}
                 {:name      :person/id
                  :generator (gen/uuid)}
                 {:name      :person/name
                  :generator (gen/uuid)}}
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

(deftest own-properties
  (let [num-cats 5
        attrs    #{{:name      :cat/name
                    :generator (gen/string)}
                   {:name      :cat/loud-name
                    :generator (fn [{[name] :dep-vals}]
                                 (string/upper-case name))
                    :deps      [[:cat/name]]}}
        world    (sut/gen attrs {:cat num-cats})
        cats     (:cat world)]
    (is (= num-cats (count cats)))
    (doseq [cat cats]
      (is (= (:cat/loud-name cat) (string/upper-case (:cat/name cat)))))))

(deftest lookup-path
  (let [barry {:cat/id     4
               :cat/name   "Barry"
               :cat/friend [:cat/id 5]}
        bobby {:cat/id   5
               :cat/name "Bobby"}
        world {:cat [barry bobby]}]
    (is (= "Bobby" (sut/lookup-path {:world world :entity barry}
                                    [:cat/friend :cat/name])))))

(deftest test-refs
  (let [attrs #{{:name      :cat/name
                 :generator (fn [{[owner-name] :dep-vals}]
                              (str owner-name "'s cat"))
                 :deps      [[:cat/owner :person/name]]}
                {:name      :cat/id
                 :generator (gen/uuid)}
                {:name      :cat/owner
                 :generator (sut/pick-ref)
                 :deps      [[:person/id]]}
                {:name      :person/id
                 :generator (gen/uuid)}
                {:name      :person/name
                 :generator (gen/one-of ["Fred"])}}
        world (sut/gen attrs {:cat 1 :person 1})]
    (is (= "Fred" (get-in world [:person 0 :person/name])))
    (is (= "Fred's cat" (get-in world [:cat 0 :cat/name])))))

(deftest test-unique-refs
  (let [attrs #{{:name      :cat/name
                 :generator (fn [{[owner-name] :dep-vals}]
                              (str owner-name "'s cat"))
                 :deps      [[:cat/owner :person/name]]}
                {:name      :cat/id
                 :generator (gen/uuid)}
                {:name      :cat/owner
                 :generator (sut/pick-unique-ref)
                 :deps      [[:person/id]]}
                {:name      :person/id
                 :generator (gen/uuid)}
                {:name        :person/name
                 :generator   (gen/one-of ["Fred" "Barney"])
                 :constraints [constraints/unique]}}
        world (sut/gen attrs {:cat 2 :person 2})]
    (is (= #{"Fred's cat" "Barney's cat"} (set (map :cat/name (:cat world)))))))
