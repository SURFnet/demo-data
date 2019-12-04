(ns nl.surf.markov-chain-test
  (:require [clojure.test :refer [deftest is testing]]
            [nl.surf.markov-chain :as sut]))

(def sample-size 100)

(deftest generate
  (testing "some examples"
    (is (every? #{[1 2 3]}
                (repeatedly sample-size #(sut/generate (sut/build-state-space [[1 2 3]]))))
        "one sample, one possible result")
    (let [state-space (sut/build-state-space [[1 2 3] [2 3 4]]
                                             :max-length 4)]
      (is (every? #{[1 2 3] [2 3 4] [2 3] [1 2 3 4]}
                  (repeatedly sample-size #(sut/generate state-space)))
          "one sample, one possible result")))
  (testing "et-cetera"
    (is (= ::sut/et-cetera
           (->> (repeatedly sample-size #(sut/generate (sut/build-state-space [[1 2 3] [2 3 3]])))
                (drop-while #(<= (count %) 3))
                first
                last))
        "running until length exceeded max length of input causes ::et-cetera at the end")
    (is (every? #(= (last %) ::sut/et-cetera)
                (repeatedly sample-size #(sut/generate (sut/build-state-space [[1 2 3] [2 3 3]]
                                                                              :max-length 1))))
        "setting max-length too tight causes ::et-cetera at the end")))
