(ns repo-analyzer.analyze
  (:import (java.time LocalDateTime)))

(use 'clj-jgit.porcelain)
(use 'clojure.tools.trace)

(defn compute-contributors-statistics
  "Creates a mapping contributor name -> contributor info from the provided logs"
  [logs]
  (let [contributor-list (flatten (map #(list (:author %) (:committer %)) logs))]
    ;(trace contributor-list)
    (reduce #(let [name (:name %2) email (:email %2)]
               (assoc %1 name {:name name :email email}))   ; TODO: names with different emails or names with same emails?
            {} contributor-list)))

(defn get-logs-by-author
  "Creates a map author -> commits from the given logs"
  [logs]
  (reduce #(let [author-name (:name (:author %2))]
             (if (contains? %1 author-name)
               (assoc %1 author-name (conj (get %1 author-name) %2))
               (assoc %1 author-name (list %2))))
          {}
          logs))

(defn get-logs-by-committer
  "Creates a map committer -> commits from the given logs"
  [logs]
  (reduce #(let [committer-name (:name (:committer %2))]
             (if (contains? %1 committer-name)
               (assoc %1 committer-name (conj (get %1 committer-name) %2))
               (assoc %1 committer-name (list %2))))
          {}
          logs))

(deftrace compute-commit-statistics
  "Computes overall commit statistics"
  [logs]
  {
   :commits           logs
   :number-of-commits (count logs)
   :self-committed    (filter #(= (:name (:author %)) (:name (:committer %))) logs)
   :committed-by-different-dev    (filter #(not ( = (:name (:author %)) (:name (:committer %)))) logs)
   })

(defn compute-meta-data
          "Computes meta data for the analysis"
          [path-to-repo]
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
                :by-author               (get-logs-by-author logs)
                :by-committer            (get-logs-by-committer logs)})))
