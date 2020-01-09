(ns nl.surf.world-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clojure.string :as string]
            [nl.surf.generators :as gen]
            [nl.surf.world :as world]
            [nl.surf.constraints :as constraints]))

(deftest sort-attrs
  (are [res attrs] (= res (world/sort-attrs attrs))
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
                        (world/sort-attrs #{{:name :a :deps [[:b]]}
                                            {:name :b :deps [[:a]]}}))
      "detect circular dependencies")

  (is (thrown-with-msg? clojure.lang.ExceptionInfo #".*dependency on undefined attribute.*"
                        (world/sort-attrs #{{:name :a :deps [[:b]]}}))
      "detect dependencies on non-existing attributes"))

(deftest test-get-entity
  (is (= {:foo/id 2 :foo/name "Fred"}
         (world/get-entity {:foo [{:foo/id 1} {:foo/id 2 :foo/name "Fred"}]}
                           [:foo/id 2]))))

(deftest gen
  (let [attrs  #{{:name      :cat/id
                  :generator (gen/uuid)}
                 {:name      :cat/owner
                  :deps      [[:person/id]]
                  :generator (world/pick-ref)}
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
        result (world/gen attrs dist)]
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
        world    (world/gen attrs {:cat num-cats})
        cats     (:cat world)]
    (is (= num-cats (count cats)))
    (doseq [cat cats]
      (is (= (:cat/loud-name cat) (string/upper-case (:cat/name cat)))))))

(deftest lookup-path
  (let [barry {:cat/id     4
               :cat/name   "Barry"
               :cat/friend [:cat/id 5]
               :cat/mother [:cat/id 1]}
        bobby {:cat/id     5
               :cat/name   "Bobby"
               :cat/mother [:cat/id 1]}
        mom   {:cat/id   1
               :cat/name "Mom"}

        world {:cat [barry bobby mom]}]
    (testing "forward refs"
      (is (= "Bobby" (world/lookup-path world barry [:cat/friend :cat/name])))
      (is (= "Mom" (world/lookup-path world barry [:cat/mother :cat/name]))))
    (testing "backrefs/joins"
      (is (= #{"Barry" "Bobby"}
             (set (world/lookup-path world mom [[:cat/mother :cat/id] :cat/name])))))))

(deftest test-ref
  (let [attrs #{{:name      :cat/name
                 :generator (fn [{[owner-name] :dep-vals}]
                              (str owner-name "'s cat"))
                 :deps      [[:cat/owner :person/name]]}
                {:name      :cat/id
                 :generator (gen/uuid)}
                {:name      :cat/owner
                 :generator (world/pick-ref)
                 :deps      [[:person/id]]}
                {:name      :person/id
                 :generator (gen/uuid)}
                {:name      :person/name
                 :generator (gen/one-of ["Fred"])}}
        world (world/gen attrs {:cat 1 :person 1})]
    (is (= "Fred" (get-in world [:person 0 :person/name])))
    (is (= "Fred's cat" (get-in world [:cat 0 :cat/name])))))

(deftest test-unique-ref
  (let [attrs #{{:name      :cat/name
                 :generator (fn [{[owner-name] :dep-vals}]
                              (str owner-name "'s cat"))
                 :deps      [[:cat/owner :person/name]]}
                {:name      :cat/id
                 :generator (gen/uuid)}
                {:name      :cat/owner
                 :generator (world/pick-unique-ref)
                 :deps      [[:person/id]]}
                {:name      :person/id
                 :generator (gen/uuid)}
                {:name        :person/name
                 :generator   (gen/one-of ["Fred" "Barney"])
                 :constraints [constraints/unique]}}
        world (world/gen attrs {:cat 2 :person 2})]
    (is (= #{"Fred's cat" "Barney's cat"} (set (map :cat/name (:cat world)))))))

(deftest test-unique-refs
  (testing "Basic unique refs"
    (let [attrs #{{:name        :cat/name
                   :generator   (gen/one-of ["Cleo" "Tiger"])
                   :constraints [constraints/unique]}
                  {:name      :cat/id
                   :generator (gen/uuid)}
                  {:name      :owner/refs
                   :generator (world/pick-unique-refs)
                   :deps      [[:cat/id] [:person/id]]}
                  ;; this is ugly but it works
                  {:name      :owner/cat
                   :deps      [[:owner/refs]]
                   :generator (fn [{[[cat-ref]] :dep-vals}] ; pick cat-ref out of vector
                                cat-ref)}
                  {:name      :owner/person
                   :deps      [[:owner/refs]]
                   :generator (fn [{[[_ person-ref]] :dep-vals}] ; pick person-ref out of vector
                                person-ref)}
                  {:name      :owner/description
                   :generator (fn [{[person-name cat-name] :dep-vals}]
                                (str "Cat " cat-name " is owned by " person-name))
                   :deps      [[:owner/person :person/name] [:owner/cat :cat/name]]}
                  {:name      :person/id
                   :generator (gen/uuid)}
                  {:name        :person/name
                   :generator   (gen/one-of ["Fred" "Barney"])
                   :constraints [constraints/unique]}}
          world (world/gen attrs {:cat 2 :person 2 :owner 4})]
      (is (= #{"Cat Cleo is owned by Barney"
               "Cat Cleo is owned by Fred"
               "Cat Tiger is owned by Barney"
               "Cat Tiger is owned by Fred"}
             (set (map :owner/description (:owner world)))))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No unique refs to.*"
                            (world/gen attrs {:cat 2 :person 2 :owner 5})))))
  (testing "with at-least-once"
    (dotimes [_ 10]
      (let [attrs #{{:name        :cat/name
                     :generator   (gen/one-of ["Cleo" "Tiger" "Lion" "Ginger"])
                     :constraints [constraints/unique]}
                    {:name      :owner/refs
                     :generator (world/pick-unique-refs [true false])
                     :deps      [[:cat/name] [:person/name]]}
                    {:name        :person/name
                     :generator   (gen/one-of ["Fred" "Barney" "Wilma" "Rubble" "Dino" "Robot"])
                     :constraints [constraints/unique]}}
            world (world/gen attrs {:cat 4 :person 6 :owner 4})]
        (is (= #{"Cleo" "Tiger" "Lion" "Ginger"}
               (set (map #(get-in % [:owner/refs 0 1]) (:owner world)))))
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No unique refs to.*"
                              (world/gen attrs {:cat 2 :person 2 :owner 5})))))))

(deftest test-joins
  (let [attrs #{{:name        :cat/name
                 :generator   (gen/one-of ["Cleo" "Tiger"])
                 :constraints [constraints/unique]}
                {:name      :owner/refs
                 :generator (world/pick-unique-refs)
                 :deps      [[:cat/name] [:person/name]]}
                ;; this is ugly but it works
                {:name      :owner/cat
                 :deps      [[:owner/refs]]
                 :generator (fn [{[[cat-ref]] :dep-vals}] ; pick cat-ref out of vector
                              cat-ref)}
                {:name      :owner/person
                 :deps      [[:owner/refs]]
                 :generator (fn [{[[_ person-ref]] :dep-vals}] ; pick person-ref out of vector
                              person-ref)}
                {:name        :person/name
                 :generator   (gen/one-of ["Fred" "Barney"])
                 :constraints [constraints/unique]}
                {:name      :person/cats
                 :deps      [[:person/name] [[:owner/person :person/name] :owner/cat :cat/name]]
                 :generator (fn [{[person-name cat-names] :dep-vals}]
                              (str person-name " owns " (string/join " and " (sort cat-names))))}}
        world (world/gen attrs {:cat 2 :person 2 :owner 4})
        fred  (world/get-entity world [:person/name "Fred"])]
    (is (= #{"Cleo" "Tiger"}
           (set (world/lookup-path world fred
                                   [[:owner/person :person/name] :owner/cat :cat/name]))))
    (is (= #{"Barney owns Cleo and Tiger"
             "Fred owns Cleo and Tiger"}
           (set (map :person/cats (:person world)))))))
