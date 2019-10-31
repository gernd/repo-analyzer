(ns repo-analyzer.render
  (:require [clojure.string :as string])
  (:import (java.security MessageDigest)
           (java.io File)))

(use 'clojure.pprint)
(use 'clojure.tools.trace)
(use 'hiccup.core)

(defn md5
  "Calculates the MD5 hash of the given string"
  [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn create-gravatar-html
  [email]
  (let [gravatar-url (string/join
                       [
                        "https://www.gravatar.com/avatar/"
                        (-> email
                            string/trim
                            string/lower-case
                            md5)
                        ])]
    (html
      [:img {:src gravatar-url}]
      )
    ))

(defn create-site
  "Creates a HTML site with basic header / footer and the given content and saves it as a file"
  [filename site-title html-content]
  (let [
        site-html (html
                    [:head
                     [:title site-title]
                     [:meta {:charset "utf-8"}]
                     [:meta {:name "viewport", :content "width=device-width, initial-scale=1, shrink-to-fit=no"}]
                     [:link {:rel         "stylesheet"
                             :href        "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
                             :integrity   "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
                             :crossorigin "anonymous"}]
                     [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.8.0/Chart.bundle.min.js"}]
                     ]
                    ; TODO include JS if needed for bootstrap
                    ;<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
                    ;<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
                    ;<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>
                    [:body html-content]
                    )
        ]
    (println "Creating " filename)
    (spit filename site-html))
  )

(defn create-commit-list-html
  "Creates HTML for a list of commits"
  [list-of-commits]
  (html
    [:ul (map #(vector :li (:msg %)) list-of-commits)
     ]
    )
  )

(defn create-contributors-ranking-html
  [contributor-rankings]
  (html
    [:h2 "Authored commits"]
    [:ol
     (map
       #(vector :li (string/join [(:name %) " - " (:authored-commits-count %)])) (:authored-commits-ranking contributor-rankings)
       )
     ]
    [:h2 "Committed commits"]
    [:ol
     (map
       #(vector :li (string/join [(:name %) " - " (:committed-commits-count %)])) (:committed-commits-ranking contributor-rankings)
       )
     ]
    [:h2 "Authored and committed commits"]
    [:ol
     (map
       #(vector :li (string/join [(:name %) " - " (:authored-and-committed-commits-count %)])) (:authored-and-committed-commits-ranking contributor-rankings)
       )
     ]
    ))

(defn create-contributor-commit-statistics
  [contributor-statistics contributor-name base-path]
  (let [
        authored-commits-list-html (create-commit-list-html (get-in contributor-statistics [contributor-name :authored-commits :commits]))
        authored-commits-list-site-name (string/join [base-path contributor-name "-authored-commits.html"])
        authored-commits-count (get-in contributor-statistics [contributor-name :authored-commits :count])
        committed-commits-list-html (create-commit-list-html (get-in contributor-statistics [contributor-name :committed-commits :commits]))
        committed-commits-count (get-in contributor-statistics [contributor-name :committed-commits :count])
        committed-commits-list-site-name (string/join [base-path contributor-name "-committed-commits.html"])
        authored-and-committed-commits-list-html (create-commit-list-html (get-in contributor-statistics [contributor-name :authored-and-committed-commits :commits]))
        authored-and-committed-commits-count (get-in contributor-statistics [contributor-name :authored-and-committed-commits :count])
        authored-and-committed-commits-list-site-name (string/join [base-path contributor-name "-authored-and-committed-commits.html"])
        ]
    (create-site authored-commits-list-site-name
                 (string/join ["Commits authored by " contributor-name]) authored-commits-list-html)
    (create-site committed-commits-list-site-name
                 (string/join ["Commits committed by " contributor-name]) committed-commits-list-html)
    (create-site authored-and-committed-commits-list-site-name
                 (string/join ["Commits authored and committed by " contributor-name]) authored-and-committed-commits-list-html)
    (html
      [:h2 contributor-name]
      [:p (create-gravatar-html (:email (get contributor-statistics contributor-name)))
       (:email (get contributor-statistics contributor-name))]
      [:p
       [:a {:href authored-commits-list-site-name} "Commits authored by " contributor-name]
       "(" authored-commits-count ")"
       ]
      [:p
       [:a {:href committed-commits-list-site-name} "Commits committed by " contributor-name]
       "(" committed-commits-count ")"
       ]
      [:p
       [:a {:href authored-and-committed-commits-list-site-name} "Commits authored and committed by " contributor-name]
       "(" authored-and-committed-commits-count ")"
       ]
      )
    ))

(defn create-contributors-statistics
  "Creates HTML for contributors statistics and creates subpages"
  [analysis base-path]
  (let [
        contributor-list (get-in analysis [:contributors-statistics :single-contributor-statistics])
        contributor-names (keys contributor-list)
        ]
    (html
      [:h1 "Contributors"]
      (map #(create-contributor-commit-statistics contributor-list % base-path) contributor-names)
      [:h1 "Rankings"]
      (create-contributors-ranking-html (get-in analysis [:contributors-statistics :rankings]))
      )))

(defn create-commit-statistics
  "Creates commit statistics HTML and subpages. Returns the created HTML"
  [analysis base-path]
  (let [commit-list-html (string/join (map #(string/join ["<li>" (:msg %) "</li>"]) (:commits (:commit-statistics analysis))))
        self-commit-list-html (string/join (map #(string/join ["<li>" (:msg %) " by " (:name (:author %)) "</li>"]) (get-in analysis [:commit-statistics :self-committed :commits])))
        different-committer-list-html (string/join (map #(string/join ["<li>" (:msg %) " authored by " (:name (:author %))
                                                                       " committed by " (:name (:committer %))
                                                                       "</li>"]) (get-in analysis [:commit-statistics :committed-by-different-dev :commits])))
        all-commits-filename (string/join [base-path "all-commits-list.html"])
        self-commits-filename (string/join [base-path "self-committed-list.html"])
        different-committer-filename (string/join [base-path "different-committer-list.html"])
        pie-chart-dataset (string/join "," [
                                            (get-in analysis [:commit-statistics :self-committed :count])
                                            (get-in analysis [:commit-statistics :committed-by-different-dev :count])])
        ]
    (create-site all-commits-filename "All commits" commit-list-html)
    (create-site self-commits-filename "Self committed commits" self-commit-list-html)
    (create-site different-committer-filename "Commits where committer and author are different" different-committer-list-html)
    (html
      [:h1 "Commit analysis"]
      [:canvas {:id "commit-chart" :width "770px" :height "385px"}]
      [:script "new Chart('commit-chart',
      {'type':'pie','data':{'labels':['Self committed','Committer and author are different'],
      'datasets':[{'label':'Committed / Authored','data': [" pie-chart-dataset "],
      'backgroundColor':['rgb(255, 99, 132)','rgb(54, 162, 235)','rgb(255, 205, 86)']}]},
      'options': {'responsive': false}});
      "]
      [:p "Commits analyzed: " (:count (:commit-statistics analysis))
       [:a {:href "all-commits-list.html"} " See list of all commits"]
       ]
      [:p "Self committed commits: " (get-in analysis [:commit-statistics :self-committed :count]) "/"
       (:count (:commit-statistics analysis)) "(" (get-in analysis [:commit-statistics :self-committed :percentage]) "%)"
       [:a {:href "self-committed-list.html"} " See list of all self-committed commits"]
       ]
      [:p "Committer and author are different: " (get-in analysis [:commit-statistics :committed-by-different-dev :count]) "/"
       (:count (:commit-statistics analysis)) "(" (get-in analysis [:commit-statistics :committed-by-different-dev :percentage]) "%)"
       [:a {:href "different-committer-list.html"} " See list of all commits where author and committer are different"]
       ]
      )
    ))

(defn create-meta-data-html
  "Creates HTML for the analysis' meta data"
  [analysis]
  (html
    [:h1 "Analysis information"]
    [:p "Repository: " (get-in analysis [:meta-data :repo-name])]
    [:p "Created: " (get-in analysis [:meta-data :creation-date])]
    )
  )

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
    (create-site index-site-name "Git repo analysis" index-site-html)
    ))

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
