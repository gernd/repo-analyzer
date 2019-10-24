(ns repo-analyzer.render
  (:require [clojure.string :as string]))

(use 'clojure.pprint)
(use 'clojure.tools.trace)


(defn create-commits-by-author-list-html
          [author by-author-map]
          (let [author-commit-list (get by-author-map author)]
            (string/join (map #(string/join ["<li>" (:msg %) "</li>"]) author-commit-list))))

(deftrace create-commits-by-committer-list-html
  [committer by-committer-map]
  (let [committer-commit-list (get by-committer-map committer)]
    (string/join (map #(string/join ["<li>" (:msg %) "</li>"]) committer-commit-list))))

(defn create-commits-by-author-html
  [analysis]
  ; keys of the by-author map are the author names
  (let [by-author-map (:by-author analysis)
        author-names (keys by-author-map)]
    (string/join (map #(string/join [
                                     "<h3>" % "</h3>"
                                     "<ul>" (create-commits-by-author-list-html % by-author-map) "</ul>"])
                      author-names))))

(deftrace create-commits-by-committer-html
  [analysis]
  (let [by-committer-map (:by-committer analysis)
        committer-names (keys by-committer-map)]
    (string/join (map #(string/join [
                                     "<h3>" % "</h3>"
                                     "<ul>" (create-commits-by-committer-list-html % by-committer-map) "</ul>"])
                      committer-names))))

(defn create-analysis-html
  "Creates HTML for a given analysis"
  [analysis]
  (let [commit-list-html (string/join (map #(string/join ["<li>" (:msg %) "</li>"]) (:logs analysis)))
        commits-by-author-html (create-commits-by-author-html analysis)
        commits-by-committer-html (create-commits-by-committer-html analysis)
        ]
    (string/join
      ["<html><head><title>Repository Analysis></title></head>"
       "<body>"
       "<h1>Analysis of the repository</h1>"
       "<h2>List of all commits</h2>"
       "<ul>" commit-list-html "</ul>"
       "<h2>List commits by author</h2>"
       commits-by-author-html
       "<h2>List commits by committer</h2>"
       commits-by-committer-html
       "</body></html>"])))

(defn render-analysis-html
  "Renders the repository analysis as HTML and saves it as a file"
  [repo-analysis]
  (println "Saving HTML report")
  (let [html "<html><head>Repository Analysis></head><body>Analysis of the repository</body>"]
    (spit "/tmp/analysis.html" (create-analysis-html repo-analysis))))

(defn render-analysis-pprint
  "Renders the repository analysis by just dumping it on the command line"
  [repo-analysis]
  (pprint repo-analysis))
