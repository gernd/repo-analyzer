(ns repo-analyzer.render
  (:require [clojure.string :as string])
  (:import (java.security MessageDigest)
           (java.io File)))

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

(defn create-site
  "Creates a HTML site with basic header / footer and the given content and saves it as a file"
  [filename site-title html-content]
  (let [
        site-html (string/join
                    ["<html>"
                     "<head>"
                     "<title>" site-title "</title>"
                     "<meta charset=\"utf-8\">"
                     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">"
                     "<link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css\" integrity=\"sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T\" crossorigin=\"anonymous\">"
                     "</head>"
                     "<body>"
                     html-content
                     ; TODO include JS if needed
                     ;<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
                     ;<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
                     ;<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>
                     "</body></html>"])
        ]
    (println "Creating " filename)
    (spit filename site-html))
  )

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

(defn create-contributor-commit-statistics
  [contributor-statistics contributor-name base-path]
  (let [
        authored-commits-list-html (create-commit-list-html (:authored-commits (get contributor-statistics contributor-name)))
        authored-commits-list-site-name (string/join [base-path contributor-name "-authored-commits.html"])
        committed-commits-list-html (create-commit-list-html (:committed-commits (get contributor-statistics contributor-name)))
        committed-commits-list-site-name (string/join [base-path contributor-name "-committed-commits.html"])
        authored-and-committed-commits-list-html (create-commit-list-html (:authored-and-committed-commits (get contributor-statistics contributor-name)))
        authored-and-committed-commits-list-site-name (string/join [base-path contributor-name "-authored-and-committed-commits.html"])
        ]
    (create-site authored-commits-list-site-name
                 (string/join ["Commits authored by " contributor-name]) authored-commits-list-html)
    (create-site committed-commits-list-site-name
                 (string/join ["Commits committed by " contributor-name]) committed-commits-list-html)
    (create-site authored-and-committed-commits-list-site-name
                 (string/join ["Commits authored and committed by " contributor-name]) authored-and-committed-commits-list-html)
    (string/join [
                  "<h2>" contributor-name "</h2>"
                  "<p>"
                  (create-gravatar-html (:email (get contributor-statistics contributor-name)))
                  (:email (get contributor-statistics contributor-name))
                  "</p>"
                  "<p>
                  <a href=\"" authored-commits-list-site-name "\">Commits authored by "contributor-name "</a>"
                  "</p>"
                  "<p>
                  <a href=\"" committed-commits-list-site-name "\">Commits committed by "contributor-name "</a>"
                  "</p>"
                  "<p>
                  <a href=\"" authored-and-committed-commits-list-site-name "\">Commits authored and committed by "contributor-name "</a>"
                  "</p>"
                  ])
    ))

(defn create-contributors-statistics
  "Creates HTML for contributors statistics and creates subpages"
  [analysis base-path]
  (let [
        contributor-list (:contributors-statistics analysis)
        contributor-names (keys contributor-list)
        ]
    (string/join [
                  "<h1>Contributors</h1>"
                  (string/join (map #(create-contributor-commit-statistics contributor-list % base-path) contributor-names))
                  ])
    ))

(defn create-commit-statistics
  "Creates HTML for commit statistics creates subpages"
  [analysis base-path]
  (let [commit-list-html (string/join (map #(string/join ["<li>" (:msg %) "</li>"]) (:commits (:commit-statistics analysis))))
        self-commit-list-html (string/join (map #(string/join ["<li>" (:msg %) " by " (:name (:author %)) "</li>"]) (:self-committed (:commit-statistics analysis))))
        different-committer-list-html (string/join (map #(string/join ["<li>" (:msg %) " authored by " (:name (:author %))
                                                                       " committed by " (:name (:committer %))
                                                                       "</li>"]) (:committed-by-different-dev (:commit-statistics analysis))))
        all-commits-filename (string/join [base-path "all-commits-list.html"])
        self-commits-filename (string/join [base-path "self-committed-list.html"])
        different-committer-filename (string/join [base-path "different-committer-list.html"])
        ]
    (create-site all-commits-filename "All commits" commit-list-html)
    (create-site self-commits-filename "Self committed commits" self-commit-list-html)
    (create-site different-committer-filename "Commits where committer and author are different" different-committer-list-html)
    (string/join
      [
       "<h1>Commit analysis</h1>"
       "<p>Total number of commits: " (:number-of-commits (:commit-statistics analysis)) "</p>"
       "<p><a href=\"all-commits-list.html\">List of all commits</a></p>"
       "<h2>Self-committed</h2>"
       "<p><a href=\"self-committed-list.html\">List of all self committed commits</a></p>"
       "<h2>Committer and author are different</h2>"
       "<p><a href=\"different-committer-list.html\">List of all commits where author and committer differ</a></p>"
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

(defn create-analysis-html-report
  "Creates the index site for the given analysis including all subpages"
  [analysis base-path]
  (let [commit-statistics-html (create-commit-statistics analysis base-path)
        contributors-html (create-contributors-statistics analysis base-path)
        meta-data-html (create-meta-data-html analysis)
        index-site-html (string/join
                          [meta-data-html
                           commit-statistics-html
                           contributors-html])
        index-site-name (string/join [base-path "index.html"])
        ]
    (create-site index-site-name "Git repo analysis" index-site-html)))

(defn render-analysis-html
  "Renders the repository analysis as HTML"
  [repo-analysis]
  (let [html-output-folder "/tmp/repo-analyzer-html/"]
    (println "Generating HTML report")
    (println "Creating folder for HTML output:" html-output-folder)
    (.mkdirs (File. html-output-folder))
    (create-analysis-html-report repo-analysis html-output-folder)))

(defn render-analysis-pprint
  "Renders the repository analysis by just dumping it on the command line"
  [repo-analysis]
  (pprint repo-analysis))
