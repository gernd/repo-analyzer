(ns repo-analyzer.core
  (:gen-class))

(use 'clj-jgit.porcelain)
(use 'clojure.pprint)

(defn analyze-repository
  "Analyzes the given GIT repository"
  [path-to-repo]
  (with-repo path-to-repo
             (pprint (git-log repo))))

(defn -main
  "I don't do a whole lot ... yet."
  [path-to-repo]
  (println "Analyzing" path-to-repo)
  (analyze-repository path-to-repo))

