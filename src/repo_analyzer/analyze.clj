(ns repo-analyzer.analyze
  (:import (java.time LocalDateTime)))

(use 'clj-jgit.porcelain)
(use 'clojure.tools.trace)

(defn compute-contributors-name-map
  "Creates a mapping contributor name -> contributor info from the provided logs"
  [logs]
  (let [contributor-list (flatten (map #(list (:author %) (:committer %)) logs))]
    ;(trace contributor-list)
    (reduce #(let [name (:name %2) email (:email %2)]
               (assoc %1 name {:name name :email email}))   ; TODO: names with different emails or names with same emails?
            {} contributor-list)))

(defn compute-contributors-statistics
  [logs]
  (println "Computing contributors statistics")
  (let [contributors-name-map (compute-contributors-name-map logs)]
    (reduce (fn [altered-map [name existing-map]]
              (let [authored-commits (filter #(and (= name (get-in % [:author :name]))
                                                   (not (= name (get-in % [:committer :name])))
                                                   ) logs)
                    committed-commits (filter #(and
                                                 (= name (get-in % [:committer :name]))
                                                 (not (= name (get-in % [:author :name])))
                                                 ) logs)
                    authored-and-committed-commits (filter #(and (= name (get-in % [:author :name]))
                                                                 (= name (get-in % [:committer :name]))) logs)
                    new-contributor-map (assoc existing-map
                                          :authored-commits authored-commits :committed-commits committed-commits
                                          :authored-and-committed-commits authored-and-committed-commits
                                          )
                    ]
                (assoc altered-map name new-contributor-map))) {} contributors-name-map)))

(defn compute-commit-statistics
  "Computes overall commit statistics"
  [logs]
  (println "Computing commit statistics")
  {
   :commits                    logs
   :number-of-commits          (count logs)
   :self-committed             (filter #(= (:name (:author %)) (:name (:committer %))) logs)
   :committed-by-different-dev (filter #(not (= (:name (:author %)) (:name (:committer %)))) logs)
   })

(defn compute-meta-data
  "Computes meta data for the analysis"
  [path-to-repo]
  (println "Computing meta data")
  {:repo-name     path-to-repo
   :creation-date (str (LocalDateTime/now))
   })

(defn analyze-repository
  "Analyzes the given GIT repository and returns the analysis"
  [path-to-repo]
  (with-repo path-to-repo
             (let [logs (git-log repo)]
               {:meta-data               (compute-meta-data path-to-repo)
                :commit-statistics       (compute-commit-statistics logs)
                :contributors-statistics (compute-contributors-statistics logs)
                })))
