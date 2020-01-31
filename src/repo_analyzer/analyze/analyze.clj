(ns repo-analyzer.analyze.analyze
  (:import (java.time LocalDateTime))
  (:require [clojure.tools.logging :as log])
  (:use [repo-analyzer.analyze.commits :only (compute-commit-statistics)]
        [repo-analyzer.util]
        [clj-jgit.porcelain]
        [clj-jgit.querying]
        [clojure.tools.trace]))

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

(defn compute-meta-data
  "Computes meta data for the analysis"
  [path-to-repo]
  (future
    (log/info "Computing meta data")
    (let [meta-data {:repo-name path-to-repo :creation-date (str (LocalDateTime/now))}]
      (log/info "Computation of meta data finished")
      meta-data)))

(defn compute-file-change-statistics
  "Computes statistics about changes on file level"
  [commits]
  (future
    (log/info "Computing file change statistics")
    (let [per-file-statistics (->> commits
                                   (map #(:additional-commit-info %))
                                   (map #(map (fn [file-change-entry]
                                                {:author  (:author %1)
                                                 :time    (:time %1)
                                                 :id      (:id %1)
                                                 :message (:message %1)
                                                 :file    (first file-change-entry)
                                                 :action  (second file-change-entry)}) (:changed_files %1)))
                                   (apply concat)
                                   (sort-by :time)
                                   (reduce #(let [action (:action %2)
                                                  filename (:file %2)
                                                  time (:time %2)
                                                  author (:author %2)
                                                  message (:message %2)]
                                              (case action
                                                :add (assoc %1 filename {:creation {:time time :author author :message message}
                                                                         :edits    []})
                                                :edit (let [existing-entry (get %1 filename)
                                                            existing-edit-list (get existing-entry :edits)
                                                            new-edit-entry {:author author :time time :message message}
                                                            new-edit-list (conj existing-edit-list new-edit-entry)
                                                            new-entry (assoc existing-entry :edits new-edit-list)]
                                                        (assoc %1 filename new-entry))
                                                :delete (let [existing-entry (get %1 filename)
                                                              new-entry (assoc existing-entry :deletion {:time time :author author :message message})]
                                                          (assoc %1 filename new-entry))
                                                %1)) {}))
          creation-statistics {:still-existing           (->> per-file-statistics
                                                              (filter #(not (contains? (second %) :deletion)))
                                                              (map #(first %)))
                               :ordered-by-creation-date (->> per-file-statistics
                                                              (sort-by (comp :time :creation second))
                                                              (map #(first %)))}

          edit-statistics {:edit-count-ranking (->> per-file-statistics
                                                    (map #(vector (first %) (count (:edits (second %)))))

                                                    (sort-by (comp second) #(compare %2 %1)))}

          deleted-files (->> per-file-statistics
                             (filter #(contains? (second %) :deletion))
                             (map #(first %)))
          file-change-statistics
          {:per-file            per-file-statistics
           :creation-statistics creation-statistics
           :edit-statistics     edit-statistics
           :deleted-files       deleted-files}]

      (log/info "Computation of file change statistics finished")
      file-change-statistics)))

(defn compute-collaboration-statistics
  "Computes collaboration statistics from the given commit-statistics"
  [commit-statistics]
  (log/info "Computing collaboration statistics")
  (let [collaboration-statistics
        (->> (:commits commit-statistics)
             (map #(vector (get-in % [:committer :email]) (get-in % [:author :email])))
             (reduce #(let [existing-committer-author-map %1
                            committer-name (first %2)
                            author-name (second %2)]
                        (if (contains? existing-committer-author-map committer-name)
                          (let [existing-set-of-authors (get existing-committer-author-map committer-name)
                                new-set-of-authors (conj existing-set-of-authors author-name)]
                            (assoc existing-committer-author-map committer-name new-set-of-authors))
                          (assoc existing-committer-author-map committer-name #{author-name}))) {}))]
    (log/info "Computation of collaboration statistics finished")
    collaboration-statistics))

(defn load-commit-logs
  "Loads the commit logs from the given folder, fetches additional information for each commit and returns
  the enriched commit log"
  [path-to-repo]
  (log/info "Loading commit logs for " path-to-repo)
  (with-repo path-to-repo
    (let [logs (git-log repo)]
      (map #(let [additional-commit-info (commit-info repo (:id %))]
              (assoc % :additional-commit-info additional-commit-info))
           logs))))

(defn analyze-commit-logs
  "Analyzes the given commit logs and returns the analysis"
  [commit-logs analyis-name]

  (let [meta-data (compute-meta-data analyis-name)
        contributors-mapping (build-contributors-mapping commit-logs)
        commit-statistics (compute-commit-statistics commit-logs)
        contributors-statistics (compute-contributors-statistics commit-logs contributors-mapping)
        file-change-statistics (compute-file-change-statistics commit-logs)]
    {:meta-data                @meta-data
     :commit-statistics        @commit-statistics
     :contributors-statistics  @contributors-statistics
     :file-change-statistics   @file-change-statistics
     :collaboration-statistics (compute-collaboration-statistics @commit-statistics)}))

(defn analyze-repository-by-folder
  "Analyzes the git repository in the given folder and returns the analysis"
  [path-to-repo]
  (let [logs (load-commit-logs path-to-repo)]
    (analyze-commit-logs logs path-to-repo)))
