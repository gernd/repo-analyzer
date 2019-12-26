(ns repo-analyzer.analyze
  (:import (java.time LocalDateTime))
  (:require [clojure.tools.logging :as log]
            [clojure.string :as string]))

(use 'clj-jgit.porcelain)
(use 'clj-jgit.querying)
(use 'clojure.tools.trace)

(defn compute-contributors-name-map
  "Creates a mapping contributor name -> contributor info from the provided logs"
  [logs]
  (let [contributor-list (flatten (map #(list (:author %) (:committer %)) logs))]
    (reduce #(let [name (:name %2) email (:email %2)]
               (assoc %1 name {:name name :email email}))   ; TODO: names with different emails or names with same emails?
            {} contributor-list)))

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
  [logs]
  (future
    (log/info "Computing contributors statistics")
    (let [contributors-name-map (compute-contributors-name-map logs)
          single-contributor-statistics
          (reduce (fn [altered-map [name existing-map]]
                    (let [authored-commits (filter #(= name (get-in % [:author :name])) logs)
                          committed-commits (filter #(= name (get-in % [:committer :name])) logs)
                          authored-and-committed-commits (filter #(and (= name (get-in % [:author :name]))
                                                                       (= name (get-in % [:committer :name]))) logs)
                          new-contributor-map (assoc existing-map
                                                     :authored-commits {:commits authored-commits :count (count authored-commits)}
                                                     :committed-commits {:commits committed-commits :count (count committed-commits)}
                                                     :authored-and-committed-commits {:commits authored-and-committed-commits
                                                                                      :count   (count authored-and-committed-commits)})]

                      (assoc altered-map name new-contributor-map))) {} contributors-name-map)
          contributor-statistics {:single-contributor-statistics single-contributor-statistics
                                  :rankings                      (compute-contributor-rankings single-contributor-statistics)}]

      (log/info "Computation of contributor statistics finished")
      contributor-statistics)))

(defn compute-commit-count-for-time
  "Computes the commit count for the given commits in intervals defined by the provided date format string"
  [commits date-format-string]

  (let [authored-commit-dates (map
                               #(vector (.format (java.text.SimpleDateFormat. date-format-string)
                                                 (get-in % [:author :date])) :authored) commits)
        committed-commit-dates (map
                                #(vector (.format (java.text.SimpleDateFormat. date-format-string)
                                                  (get-in % [:committer :date])) :committed) commits)
        all-commit-dates (concat authored-commit-dates committed-commit-dates)]
    (->> all-commit-dates
         (reduce
          #(let [commit-time-distribution %1
                 date (first %2)
                 commit-type (second %2)]
             (if (contains? commit-time-distribution date)
               (let
                [number-of-authored-commits (get-in commit-time-distribution [date :authored])
                 number-of-committed-commits (get-in commit-time-distribution [date :committed])]
                 (if (= commit-type :authored)
                   (assoc commit-time-distribution date {:authored (inc number-of-authored-commits) :committed number-of-committed-commits})
                   (assoc commit-time-distribution date {:authored number-of-authored-commits :committed (inc number-of-committed-commits)})))

               (if (= commit-type :authored)
                 (assoc commit-time-distribution date {:authored 1 :committed 0})
                 (assoc commit-time-distribution date {:authored 0 :committed 1}))))

          {})
         (sort #(compare (first %1) (first %2))))))

(defn compute-commit-count-by-day
  "Computes the commit count per day for authored and committed commits"
  [commits]
  (compute-commit-count-for-time commits "yyyy/MM/dd"))

(defn compute-commit-count-by-week
  "Computes the commit count per week for authored and committed commits"
  [commits]
  (compute-commit-count-for-time commits "yyyy/ww"))

(defn compute-commit-count-by-month
  "Computes the commit count per month for authored and committed commits"
  [commits]
  (compute-commit-count-for-time commits "yyyy/MM"))

(defn compute-commit-count-by-year
  "Computes the commit count per year for authored and committed commits"
  [commits]
  (compute-commit-count-for-time commits "yyyy"))

(defn compute-commit-daytime-distribution
  "Computes the distribution of the given commits regarding the daytime they were authored"
  [commits]
  (let [commits-with-author-time (map #(let [author-commit-date (get-in % [:author :date])
                                             author-commit-time-formatted (.format (java.text.SimpleDateFormat. "yyyy/MM/dd HH:mm") author-commit-date)
                                             author-commit-hour (Integer/parseInt (.format (java.text.SimpleDateFormat. "HH") author-commit-date))]
                                         (assoc % :author-commit-hour author-commit-hour :author-commit-time author-commit-time-formatted)) commits)
        early-commits (filter #(< (:author-commit-hour %) 8) commits-with-author-time)
        late-commits (filter #(> (:author-commit-hour %) 18) commits-with-author-time)
        regular-commits (filter #(and
                                  (<= (:author-commit-hour %) 18)
                                  (>= (:author-commit-hour %) 8) commits-with-author-time))]
    {:early-commits   early-commits
     :late-commits    late-commits
     :regular-commits regular-commits}))

(defn compute-commit-day-of-week-distribution
  "Compute distribution of commits on working days vs. commits authored on the weekend"
  [commits]
  (let [commits-with-day-of-week (map #(let [author-commit-date (get-in % [:author :date])
                                             day-of-week (.format (java.text.SimpleDateFormat. "E" (. java.util.Locale ENGLISH)) author-commit-date)]
                                         (assoc % :day-of-week day-of-week)) commits)]
    {:commits-on-weekend      (filter #(or (= "Sat" (:day-of-week %)) (= "Sun" (:day-of-week %))) commits-with-day-of-week)
     :commits-on-working-days (filter #(not (or (= "Sat" (:day-of-week %)) (= "Sun" (:day-of-week %)))) commits-with-day-of-week)}))

(defn compute-commit-message-length-ranking
  "Computes a ranking regarding the length of the commit messages"
  [logs]
  (->> logs
       (map #(hash-map :message (:msg %) :length (count (:msg %)) :author (get-in % [:author :email])))
       (sort-by :length #(compare %2 %1))))

(defn compute-commit-statistics
  "Computes overall commit statistics"
  [logs]
  (future
    (log/info "Computing commit statistics")
    (let [self-committed-commits (filter #(= (:name (:author %)) (:name (:committer %))) logs)
          committed-by-different-dev (filter #(not (= (:name (:author %)) (:name (:committer %)))) logs)
          commit-statistics
          {:commits                       logs
           :count-statistics              {:total-count (count logs)
                                           :count-by-day (compute-commit-count-by-day logs)
                                           :count-by-week (compute-commit-count-by-week logs)
                                           :count-by-month (compute-commit-count-by-month logs)
                                           :count-by-year (compute-commit-count-by-year logs)}

           :self-committed                {:commits    self-committed-commits
                                           :count      (count self-committed-commits)
                                           :percentage (* 100 (double (/ (count self-committed-commits) (count logs))))}
           :committed-by-different-dev    {:commits    committed-by-different-dev
                                           :count      (count committed-by-different-dev)
                                           :percentage (* 100 (double (/ (count committed-by-different-dev) (count logs))))}
           :time                          {:time-of-day-distribution  (compute-commit-daytime-distribution logs)
                                           :day-of-week-distribution  (compute-commit-day-of-week-distribution logs)}

           :commit-message-length-ranking (compute-commit-message-length-ranking logs)}]

      (log/info "Computation of commit statistics finished")
      commit-statistics)))

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
  [repo commits]
  (future
    (log/info "Computing file change statistics")
    (let [per-file-statistics (->> commits
                                   (map #(commit-info repo (:id %)))
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

(defn analyze-repository
  "Analyzes the given GIT repository and returns the analysis"
  [path-to-repo]
  (with-repo path-to-repo
    (let [logs (git-log repo)
          meta-data (compute-meta-data path-to-repo)
          commit-statistics (compute-commit-statistics logs)
          contributors-statistics (compute-contributors-statistics logs)
          file-change-statistics (compute-file-change-statistics repo logs)]
      {:meta-data                @meta-data
       :commit-statistics        @commit-statistics
       :contributors-statistics  @contributors-statistics
       :file-change-statistics   @file-change-statistics
       :collaboration-statistics (compute-collaboration-statistics @commit-statistics)})))
