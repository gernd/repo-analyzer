(ns repo-analyzer.core
  (:gen-class))

(use 'repo-analyzer.analyze.analyze)
(use 'repo-analyzer.render)

(defn -main
  "Analyzes the repository and creates HTML output"
  [path-to-repo html-output-dir]
  (println "Analyzing" path-to-repo)
  (let [analysis (analyze-repository path-to-repo)]
    (render-analysis-html analysis html-output-dir))
  (shutdown-agents))

