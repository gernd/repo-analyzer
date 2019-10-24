(ns repo-analyzer.analyze)

(use 'clj-jgit.porcelain)

(defn get-logs-by-author
  "Creates a map author -> commits from the given logs"
  [logs]
  (reduce #(let [author-name (:name (:author %2))]
             (if (contains? %1 author-name)
               (assoc %1 author-name (cons (get %1 author-name) %2))
               (assoc %1 author-name (list %2))))
          {}
          logs))

(defn analyze-repository
  "Analyzes the given GIT repository and returns the analysis"
  [path-to-repo]
  (with-repo path-to-repo
             (let [logs (git-log repo)]
               {:logs      logs
                :by-author (get-logs-by-author logs)})))
