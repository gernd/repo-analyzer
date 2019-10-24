(ns repo-analyzer.core
  (:gen-class))

(use 'clojure.pprint)
(use 'clojure.tools.trace)
(use 'repo-analyzer.analyze)


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

