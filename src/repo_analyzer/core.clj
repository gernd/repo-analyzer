(ns repo-analyzer.core
  (:gen-class))

(use 'repo-analyzer.analyze)
(use 'repo-analyzer.render)

(defn -main
  "I don't do a whole lot ... yet."
  [path-to-repo]
  (println "Analyzing" path-to-repo)
  (let [analysis (analyze-repository path-to-repo)]
    (render-analysis-html analysis))
  (shutdown-agents))

