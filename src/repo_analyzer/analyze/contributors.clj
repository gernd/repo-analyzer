(ns repo-analyzer.analyze.contributors
  (:require [clojure.tools.logging :as log])
  (:use [repo-analyzer.util]))

(defn build-contributors-mapping
  "Collects all contributors from the commit logs and creates IDs for them.
          The email address is considered unique.
          "
  [commit-logs]
  (let [contributor-list (flatten (map #(list (:author %) (:committer %)) commit-logs))]
    (reduce #(let [name (:name %2) email (:email %2) contributor-id (md5 email)]
               (if (some
                    (fn [existing-contributors-map] (= email (:email existing-contributors-map))) %1)
                 %1
                 (conj %1 {:name name :email email :id contributor-id})))

            [] contributor-list)))

(defn get-id-for-contributor-email
  [contributor-mapping contributor-email]
  (:id (first (filter #(= contributor-email (:email %)) contributor-mapping))))

(defn compute-contributor-rankings
  "Computes rankings from the single contributors statistics"
  [single-contributor-statistics]
  (let [contributor-commit-counts
        (map #(hash-map :name (first %)
                        :id (:id (second %))
                        :authored-commits-count (get-in (second %) [:authored-commits :count])
                        :committed-commits-count (get-in (second %) [:committed-commits :count])
                        :authored-and-committed-commits-count (get-in (second %) [:authored-and-committed-commits :count]))
             single-contributor-statistics)]
    {:authored-commits-ranking
     (->>
      contributor-commit-counts
      (filter #(> (:authored-commits-count %) 0))
      (sort-by :authored-commits-count #(compare %2 %1)))
     :committed-commits-ranking
     (->>
      contributor-commit-counts
      (filter #(> (:committed-commits-count %) 0))
      (sort-by :committed-commits-count #(compare %2 %1)))
     :authored-and-committed-commits-ranking
     (->>
      contributor-commit-counts
      (filter #(> (:authored-and-committed-commits-count %) 0))
      (sort-by :authored-and-committed-commits-count #(compare %2 %1)))}))

(defn compute-contributors-statistics
  [logs contributors-mapping]
  (future
    (log/info "Computing contributors statistics")
    (let [single-contributor-statistics
          (reduce (fn [altered-map existing-user-map]
                    (let [name (:name existing-user-map)
                          authored-commits (filter #(= name (get-in % [:author :name])) logs)
                          committed-commits (filter #(= name (get-in % [:committer :name])) logs)
                          authored-and-committed-commits (filter #(and (= name (get-in % [:author :name]))
                                                                       (= name (get-in % [:committer :name]))) logs)
                          new-contributor-map (assoc existing-user-map
                                                     :authored-commits {:commits authored-commits :count (count authored-commits)}
                                                     :committed-commits {:commits committed-commits :count (count committed-commits)}
                                                     :authored-and-committed-commits {:commits authored-and-committed-commits
                                                                                      :count   (count authored-and-committed-commits)})]
                      (assoc altered-map name new-contributor-map))) {} contributors-mapping)
          contributor-statistics {:single-contributor-statistics single-contributor-statistics
                                  :rankings                      (compute-contributor-rankings single-contributor-statistics)}]
      (log/info "Computation of contributor statistics finished")
      contributor-statistics)))
