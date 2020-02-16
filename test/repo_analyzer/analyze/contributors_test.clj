(ns repo-analyzer.analyze.contributors-test
  (:require [clojure.test :refer :all]
            [repo-analyzer.analyze.contributors :refer :all]
            [repo-analyzer.analyze.analyze :refer :all]))

(deftest test-build-contributor-mapping-one-commit-log
  (testing "Tests the build-contributor-mapping function for one provided commit log"
    (let [first-commit-of-repo (last (load-commit-logs "."))
          contributors-mapping (build-contributors-mapping [first-commit-of-repo])]
      (is (= (first contributors-mapping) {:name "Gernot Pointner", :email "gernot.pointner@googlemail.com", :id "3293afe66339df19e3959f993ea8e4c9"}))
      (is (= (second contributors-mapping) {:name "GitHub", :email "noreply@github.com", :id "9181eb84f9c35729a3bad740fb7f9d93"})))))

(deftest test-build-contributor-mapping-two-commit-logs
  (testing "Tests the build-contributor-mapping function for two provided commit log"
    (let [first-two-commits-of-repo (take-last 2 (load-commit-logs "."))
          contributors-mapping (build-contributors-mapping first-two-commits-of-repo)]
      (is (= 3 (count contributors-mapping)))
      (is (= (first contributors-mapping) {:name "Gernot Pointner", :email "gernot.pointner@jambit.com", :id "4da290e58ade4571baad8f776cbd97b8"}))
      (is (= (second contributors-mapping) {:name "Gernot Pointner", :email "gernot.pointner@googlemail.com", :id "3293afe66339df19e3959f993ea8e4c9"}))
      (is (= (nth contributors-mapping 2) {:name "GitHub", :email "noreply@github.com", :id "9181eb84f9c35729a3bad740fb7f9d93"})))))

(deftest test-get-id-for-contributor-email
  (testing "Tests the get-id-for-contributor-email function"
    (let [contributor-mapping [{:name "First User" :email "firstuser@test.com" :id "TESTID123"}]]
      (is (= "TESTID123" (get-id-for-contributor-email contributor-mapping "firstuser@test.com"))))))

