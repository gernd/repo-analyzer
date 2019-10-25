(ns repo-analyzer.render
  (:require [clojure.string :as string])
  (:import (java.security MessageDigest)))

(use 'clojure.pprint)
(use 'clojure.tools.trace)


(defn create-commits-by-author-list-html
  [author by-author-map]
  (let [author-commit-list (get by-author-map author)]
    (string/join (map #(string/join ["<li>" (:msg %) "</li>"]) author-commit-list))))

(defn create-commits-by-committer-list-html
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

(defn create-commits-by-committer-html
  [analysis]
  (let [by-committer-map (:by-committer analysis)
        committer-names (keys by-committer-map)]
    (string/join (map #(string/join [
                                     "<h3>" % "</h3>"
                                     "<ul>" (create-commits-by-committer-list-html % by-committer-map) "</ul>"])
                      committer-names))))

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(deftrace create-gravatar-html
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


(deftrace create-contributors-html
          [analysis]
          (let [contributor-list (:contributors analysis)
                contributor-names (keys contributor-list)
                ]
            (string/join
              (map #(
                      string/join ["<p>"
                                   %
                                   " Email: " (:email (get contributor-list %))
                                   " Gravatar: " (create-gravatar-html (:email (get contributor-list %)))
                                   "</p>"]) contributor-names))))

(defn create-analysis-html
  "Creates HTML for a given analysis"
  [analysis]
  (let [commit-list-html (string/join (map #(string/join ["<li>" (:msg %) "</li>"]) (:commits (:commit-statistics analysis))))
        commits-by-author-html (create-commits-by-author-html analysis)
        commits-by-committer-html (create-commits-by-committer-html analysis)
        contributors-html (create-contributors-html analysis)
        ]
    (string/join
      ["<html>"
       "<head>"
       "<title>Repository Analysis></title>"
       "<meta charset=\"utf-8\">"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">"
       "<link rel=\"stylesheet\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css\" integrity=\"sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T\" crossorigin=\"anonymous\">"
       "</head>"
       "<body>"
       "<h1>Commit analysis</h1>"
       "<h2>List of all commits</h2>"
       "<ul>" commit-list-html "</ul>"
       "<h2>List commits by author</h2>"
       commits-by-author-html
       "<h2>List commits by committer</h2>"
       commits-by-committer-html
       "<h1>Contributors</h1>"
       contributors-html
       ; TODO include JS if needed
       ;<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
       ;<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
       ;<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>
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
