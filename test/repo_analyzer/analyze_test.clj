(ns repo-analyzer.analyze-test
  (:require [clojure.test :refer :all]
            [repo-analyzer.analyze :refer :all]
            [clojure.tools.trace :refer :all]))

(deftest test-analyze-repository-integrity-commit-statistics
  (testing "Checks the commit-statistics analyze-repository function using the project's own repository by
  checking some simple constraints"
    (let [result (analyze-repository ".")]
      (is (> (count (:commits (:commit-statistics result))) 0))
      (is (= (count (:commits (:commit-statistics result))) (:number-of-commits (:commit-statistics result))))
      )))
