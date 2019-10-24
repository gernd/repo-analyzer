(ns repo-analyzer.analyze-test
  (:require [clojure.test :refer :all]
            [repo-analyzer.analyze :refer :all]
            [clojure.tools.trace :refer :all]))

(deftest test-analyze-repository
  (testing "Tests the analyze-repository function using the project's repository"
    (let [result (analyze-repository ".")]
      (is (> (count (:logs result)) 0)))))