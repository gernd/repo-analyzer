(ns repo-analyzer.render
  (:require [clojure.string :as string])
  (:import (java.security MessageDigest)))

(use 'clojure.pprint)
(use 'clojure.tools.trace)

(defn md5
  "Calculates the MD5 hash of the given string"
  [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn create-gravatar-html
  [email]
  (string/join
    [
     "<img src=\"https://www.gravatar.com/avatar/"
     (-> email
         string/trim
         string/lower-case
         md5)
     "\" />"
     ]))

(defn create-commit-list-html
  "Creates HTML for a list of commits"
  [list-of-commits]
  (string/join
    [
     "<ul>"
     (string/join (map #(string/join ["<li>" (:msg %) "</li>"]) list-of-commits))
     "</ul>"
     ])
  )

(defn create-contributors-html
  "Creates HTML for contributors statistics"
  [analysis]
  (let [contributor-list (:contributors-statistics analysis)
        contributor-names (keys contributor-list)
        ]
    (string/join
      [
       "<h1>Contributors</h1>"
       (string/join (map #(string/join ["<h2>" % "</h2>"
                                        "<p>"
                                        (create-gravatar-html (:email (get contributor-list %)))
                                        (:email (get contributor-list %))
                                        "</p>"
                                        "<h3>Authored</h3>"
                                        (create-commit-list-html (:authored-commits (get contributor-list %)))
                                        "<h3>Committed</h3>"
                                        (create-commit-list-html (:committed-commits (get contributor-list %)))
                                        "<h3>Authored and committed</h3>"
                                        (create-commit-list-html (:authored-and-committed-commits (get contributor-list %)))
                                        ])
                         contributor-names))
       ]
      )))


(defn create-commit-statistics-html
  "Creates HTML for commit statistics"
  [analysis]
  (let [commit-list-html (string/join (map #(string/join ["<li>" (:msg %) "</li>"]) (:commits (:commit-statistics analysis))))
        self-commit-list-html (string/join (map #(string/join ["<li>" (:msg %) " by " (:name (:author %)) "</li>"]) (:self-committed (:commit-statistics analysis))))
        different-committer-list-html (string/join (map #(string/join ["<li>" (:msg %) " authored by " (:name (:author %))
                                                                       " committed by " (:name (:committer %))
                                                                       "</li>"]) (:committed-by-different-dev (:commit-statistics analysis))))
        ]
    (string/join
      [
       "<h1>Commit analysis</h1>"
       "<p>Total number of commits: " (:number-of-commits (:commit-statistics analysis)) "</p>"
       "<h2>List of all commits</h2>"
       "<ul>"
       commit-list-html
       "</ul>"
       "<h2>Self-committed</h2>"
       "<ul>"
       self-commit-list-html
       "</ul>"
       "<h2>Committer and author are different</h2>"
       "<ul>"
       different-committer-list-html
       "</ul>"
       ])
    ))

(defn create-meta-data-html
  "Creates HTML for the analysis' meta data"
  [analysis]
  (string/join
    [
     "<h1>Analysis information</h1>"
     "<p>Repository: " (get-in analysis [:meta-data :repo-name]) "</p>"
     "<p>Created: " (get-in analysis [:meta-data :creation-date]) "</p>"
     ]

    ))

(defn create-analysis-html
  "Creates HTML for a given analysis"
  [analysis]
  (let [commit-statistics-html (create-commit-statistics-html analysis)
        contributors-html (create-contributors-html analysis)
        meta-data-html (create-meta-data-html analysis)
        ]
    (string/join
      ["<html>"
       "<head>"
       "<title>Repository Analysis</title>"
       "<meta charset=\"utf-8\">"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">"
       "<link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css\" integrity=\"sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T\" crossorigin=\"anonymous\">"
       "</head>"
       "<body>"
       meta-data-html
       commit-statistics-html
       contributors-html
       ; TODO include JS if needed
       ;<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
       ;<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
       ;<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>
       "</body></html>"])))

(defn render-analysis-html
  "Renders the repository analysis as HTML and saves it as a file"
  [repo-analysis]
  (let [output-file "/tmp/analysis.html"]
    (println "Saving HTML report to" output-file)
    (let [html "<html><head>Repository Analysis></head><body>Analysis of the repository</body>"]
      (spit output-file (create-analysis-html repo-analysis)))))

(defn render-analysis-pprint
  "Renders the repository analysis by just dumping it on the command line"
  [repo-analysis]
  (pprint repo-analysis))
