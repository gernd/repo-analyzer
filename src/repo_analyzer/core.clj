(ns repo-analyzer.core
  (:gen-class))

(use 'clj-jgit.porcelain)
(use 'clojure.pprint)
(use 'clojure.tools.trace)

(defn get-logs-by-author
  "Creates a map author -> commits from the given logs"
  [logs]
  (reduce #(let [author-name (:name (:author %2))]
             (if (contains? %1 author-name)
               (assoc %1 author-name (cons (get %1 author-name) %2))
               (assoc %1 author-name ())))
          {}
          logs))


(defn analyze-repository
  "Analyzes the given GIT repository and returns the analysis"
  [path-to-repo]
  (with-repo path-to-repo
             (let [logs (git-log repo)]
               {:logs      logs
                :by-author (get-logs-by-author logs)})))

(defn render-analysis
  "Renders the given analysis"
  [repo-analysis]
  (pprint repo-analysis))

(defn -main
  "I don't do a whole lot ... yet."
  [path-to-repo]
  (println "Analyzing" path-to-repo)
  (let [analysis (analyze-repository path-to-repo)]
    (render-analysis analysis)))

