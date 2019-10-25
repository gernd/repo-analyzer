(ns repo-analyzer.analyze)

(use 'clj-jgit.porcelain)
(use 'clojure.tools.trace)

(defn get-contributors
  "Creates a mapping contributor name -> contributor info from the provided logs"
  [logs]
  (let [contributor-list (flatten (map #(list (:author %) (:committer %)) logs))]
    ;(trace contributor-list)
    (reduce #(let [name (:name %2) email (:email %2)]
               (assoc %1 name {:name name :email email}))              ; TODO: names with different emails or names with same emails?
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

(defn analyze-repository
  "Analyzes the given GIT repository and returns the analysis"
  [path-to-repo]
  (with-repo path-to-repo
             (let [logs (git-log repo)]
               {:logs         logs
                :contributors (get-contributors logs)
                :by-author    (get-logs-by-author logs)
                :by-committer (get-logs-by-committer logs)})))
