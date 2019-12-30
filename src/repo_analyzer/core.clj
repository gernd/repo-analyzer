(ns repo-analyzer.core
  (:gen-class))

(use 'repo-analyzer.analyze.analyze)
(use 'repo-analyzer.render.render)

(defn -main
  "Analyzes the repository and creates HTML output"
  [& args]
  (if (or (nil? args) (< (count args) 2))
    (do
      (println "Usage: <path-of-repo-to-analyze> <html-output-dir>")
      (System/exit 1)))
  (let [path-to-repo (first args)
        html-output-dir (second args)]
    (println "Analyzing" path-to-repo)
    (let [analysis (analyze-repository path-to-repo)]
      (render-analysis-html analysis html-output-dir))
    (shutdown-agents)))

