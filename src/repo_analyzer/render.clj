(ns repo-analyzer.render)

(use 'clojure.pprint)

(defn render-analysis-pprint
  "Renders the given analysis"
  [repo-analysis]
  (pprint repo-analysis))
