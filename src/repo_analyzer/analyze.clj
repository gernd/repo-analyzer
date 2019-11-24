(ns repo-analyzer.analyze
  (:import (java.time LocalDateTime)))

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
        (map #(
                hash-map :name (first %)
                         :authored-commits-count (get-in (second %) [:authored-commits :count])
                         :committed-commits-count (get-in (second %) [:committed-commits :count])
                         :authored-and-committed-commits-count (get-in (second %) [:authored-and-committed-commits :count])
                         )
             single-contributor-statistics
             )]
    {
     :authored-commits-ranking
     (->>
       contributor-commit-counts
       (filter #(> (:authored-commits-count %) 0))
       (sort-by :authored-commits-count #(compare %2 %1))
       )
     :committed-commits-ranking
     (->>
       contributor-commit-counts
       (filter #(> (:committed-commits-count %) 0))
       (sort-by :committed-commits-count #(compare %2 %1))
       )
     :authored-and-committed-commits-ranking

     (->>
       contributor-commit-counts
       (filter #(> (:authored-and-committed-commits-count %) 0))
       (sort-by :authored-and-committed-commits-count #(compare %2 %1))
       )
     }
    ))

(defn compute-contributors-statistics
  [logs]
  (println "Computing contributors statistics")
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
                                              :authored-and-committed-commits {
                                                                               :commits authored-and-committed-commits
                                                                               :count   (count authored-and-committed-commits)}
                                              )
                        ]
                    (assoc altered-map name new-contributor-map))) {} contributors-name-map)]
    {
     :single-contributor-statistics single-contributor-statistics
     :rankings                      (compute-contributor-rankings single-contributor-statistics)
     }
    ))

(defn compute-commit-time-distribution
  [commits]
  (let [
        authored-commit-dates (map
                                #(vector (.format (java.text.SimpleDateFormat. "yyyy/MM/dd")
                                                  (get-in % [:author :date])) :authored) commits)
        committed-commit-dates (map
                                 #(vector (.format (java.text.SimpleDateFormat. "yyyy/MM/dd")
                                                   (get-in % [:committer :date])) :committed) commits)
        all-commit-dates (concat authored-commit-dates committed-commit-dates)
        ]
    (->> all-commit-dates
         (reduce
           #(let [commit-time-distribution %1
                  date (first %2)
                  commit-type (second %2)
                  ]
              (if (contains? commit-time-distribution date)
                (let
                  [
                   number-of-authored-commits (get-in commit-time-distribution [date :authored])
                   number-of-committed-commits (get-in commit-time-distribution [date :committed])
                   ]
                  (if (= commit-type :authored)
                    (assoc commit-time-distribution date {:authored (inc number-of-authored-commits) :committed number-of-committed-commits})
                    (assoc commit-time-distribution date {:authored number-of-authored-commits :committed (inc number-of-committed-commits)})
                    )
                  )
                (if (= commit-type :authored)
                  (assoc commit-time-distribution date {:authored 1 :committed 0})
                  (assoc commit-time-distribution date {:authored 0 :committed 1})
                  )
                )
              )
           {})
         (sort #(compare (first %1) (first %2)))
         )
    ))

(defn compute-commit-statistics
  "Computes overall commit statistics"
  [logs]
  (println "Computing commit statistics")
  (let [
        self-committed-commits (filter #(= (:name (:author %)) (:name (:committer %))) logs)
        committed-by-different-dev (filter #(not (= (:name (:author %)) (:name (:committer %)))) logs)
        ]
    {
     :commits                    logs
     :count                      (count logs)
     :self-committed             {
                                  :commits    self-committed-commits
                                  :count      (count self-committed-commits)
                                  :percentage (* 100 (double (/ (count self-committed-commits) (count logs))))
                                  }
     :committed-by-different-dev {
                                  :commits    committed-by-different-dev
                                  :count      (count committed-by-different-dev)
                                  :percentage (* 100 (double (/ (count committed-by-different-dev) (count logs))))
                                  }
     :time-distribution          (compute-commit-time-distribution logs)
     }
    ))

(defn compute-meta-data
  "Computes meta data for the analysis"
  [path-to-repo]
  (println "Computing meta data")
  {:repo-name     path-to-repo
   :creation-date (str (LocalDateTime/now))
   })

(defn compute-file-change-statistics
  "Computes statistics about changes on file level"
  [repo commits]
  (println "Computing file change statistics")
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
                                 (reduce #(let [
                                                action (:action %2)
                                                filename (:file %2)
                                                time (:time %2)
                                                author (:author %2)
                                                message (:message %2)
                                                ]
                                            (case action
                                              :add (assoc %1 filename {
                                                                       :creation {:time time :author author :message message}
                                                                       :edits    []
                                                                       })
                                              :edit (let [
                                                          existing-entry (get %1 filename)
                                                          existing-edit-list (get existing-entry :edits)
                                                          new-edit-entry {:author author :time time :message message}
                                                          new-edit-list (conj existing-edit-list new-edit-entry)
                                                          new-entry (assoc existing-entry :edits new-edit-list)
                                                          ]
                                                      (assoc %1 filename new-entry))
                                              :delete (let [
                                                            existing-entry (get %1 filename)
                                                            new-entry (assoc existing-entry :deletion {:time time :author author :message message})
                                                            ]
                                                        (assoc %1 filename new-entry))
                                              %1
                                              )) {})
                                 )
        creation-statistics {
                             :still-existing           (->> per-file-statistics
                                                            (filter #(not (contains? (second %) :deletion)))
                                                            (map #(first %))
                                                            )
                             :ordered-by-creation-date (->> per-file-statistics
                                                            (sort-by (comp :time :creation second))
                                                            (map #(first %))
                                                            )
                             }
        edit-statistics {
                         :edit-count-ranking (->> per-file-statistics
                                                  (map #(vector (first %) (count (:edits (second %)))))

                                                  (sort-by (comp second) #(compare %2 %1))
                                                  )
                         }
        deleted-files (->> per-file-statistics
                           (filter #(contains? (second %) :deletion))
                           (map #(first %))
                           )
        ]
    {
     :per-file            per-file-statistics
     :creation-statistics creation-statistics
     :edit-statistics     edit-statistics
     :deleted-files       deleted-files
     }
    )
  )

(deftrace compute-collaboration-statistics
          "Computes collaboration statistics from the given commit-statistics"
          [commit-statistics]
          (->> (:commits commit-statistics)
               (map #(vector (get-in % [:committer :email]) (get-in % [:author :email])))
               ))


(defn analyze-repository
  "Analyzes the given GIT repository and returns the analysis"
  [path-to-repo]
  (with-repo path-to-repo
             (let [logs (git-log repo)
                   commit-statistics (compute-commit-statistics logs)
                   ]
               {:meta-data                (compute-meta-data path-to-repo)
                :commit-statistics        commit-statistics
                :contributors-statistics  (compute-contributors-statistics logs)
                :file-change-statistics   (compute-file-change-statistics repo logs)
                :collaboration-statistics (compute-collaboration-statistics commit-statistics)
                })))
