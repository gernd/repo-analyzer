(ns repo-analyzer.analyze.analyze-test
  (:require [clojure.test :refer :all]
            [repo-analyzer.analyze.analyze :refer :all]
            [clojure.tools.trace :refer :all]))

(deftest test-analyze-repository-commit-statistics-integrity
  (testing "Checks the commit-statistics analyze-repository function using the project's own repository by
  checking some simple constraints"
    (let [result (analyze-repository-by-folder ".")]
      (is (> (count (:commits (:commit-statistics result))) 0))
      (is (= (count (:commits (:commit-statistics result))) (get-in result [:commit-statistics :count-statistics :total-count])))
      (is (= (:repo-name (:meta-data result)) ".")))))

