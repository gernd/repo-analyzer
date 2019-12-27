(ns repo-analyzer.util-test
  (:require [clojure.test :refer :all]
            [repo-analyzer.util :refer :all]))

(deftest test-md5-empty-string
  (testing "Checks that the md5 hashing function returns a hash for an empty string"
    (let [result (md5 "")]
      (is (> (count result) 0)))))

(deftest test-md5-non-empty-string
  (testing "Checks that the md5 hashing function returns a hash for an non empty string"
    (let [result (md5 "this is a not empty string")]
      (is (> (count result) 0)))))

(deftest test-md5-same-hash-for-same-string
  (testing "Checks that the md5 hashing function returns the same hash when applied to the same string"
    (let [first-result (md5 "banana")
          second-result (md5 "banana")]
      (is (= first-result second-result)))))

(deftest test-md5-same-hash-for-same-string
  (testing "Checks that the md5 hashing function returns different hashes when applied to different strings"
    (let [first-result (md5 "banana")
          second-result (md5 "apple")]
      (is (not (= first-result second-result))))))

