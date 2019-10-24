(ns repo-analyzer.core
  (:gen-class))

(use 'clj-jgit.porcelain)
(use 'clojure.pprint)

(defn analyze-repository
  "Analyzes the given GIT repository and returns the analysis"
  [path-to-repo]
  (with-repo path-to-repo
             {:logs (git-log repo)}))

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

