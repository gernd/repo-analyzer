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
    [:table {:class "table table-striped"}
     [:thead
      [:tr
       [:th {:scope "col"} "Message"]
       [:th {:scope "col"} "Author"]
       [:th {:scope "col"} "Committer"]
       ]
      ]
     [:tbody
      (map #(vector
              :tr
              [:td (:msg %)]
              [:td (get-in % [:author :name])]
              [:td (get-in % [:committer :name])]

              ) list-of-commits)
      ]
     ]
    )
  )

(defn create-contributors-ranking-html
  [contributor-rankings]
  (let [
        top-5-authored (take 5 (:authored-commits-ranking contributor-rankings))
        top-5-committed (take 5 (:committed-commits-ranking contributor-rankings))
        top-5-authored-and-committed (take 5 (:authored-and-committed-commits-ranking contributor-rankings))
        ]
    (html
      [:h2 "Authored commits"]
      [:ol
       (map
         #(vector :li (string/join [(:name %) " - " (:authored-commits-count %)])) top-5-authored
         )
       ]
      [:h2 "Committed commits"]
      [:ol
       (map
         #(vector :li (string/join [(:name %) " - " (:committed-commits-count %)])) top-5-committed
         )
       ]
      [:h2 "Authored and committed commits"]
      [:ol
       (map
         #(vector :li (string/join [(:name %) " - " (:authored-and-committed-commits-count %)])) top-5-authored-and-committed
         )
       ]
      )))

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
      [:tr
       [:td (create-gravatar-html (:email (get contributor-statistics contributor-name)))]
       [:td contributor-name]
       [:td (:email (get contributor-statistics contributor-name))]
       [:td
        [:a {:href authored-commits-list-site-name} authored-commits-count]
        ]
       [:td
        [:a {:href committed-commits-list-site-name} committed-commits-count]
        ]
       [:td
        [:a {:href authored-and-committed-commits-list-site-name} authored-and-committed-commits-count]
        ]
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
      [:table {:class "table table-striped"}
       [:thead
        [:tr
         [:th {:scope "col"} "Gravatar"]
         [:th {:scope "col"} "Name"]
         [:th {:scope "col"} "Email"]
         [:th {:scope "col"} "# of authored commits"]
         [:th {:scope "col"} "# of committed commits"]
         [:th {:scope "col"} "# of authored and committed commits"]
         ]
        ]
       [:tbody
        (map #(create-contributor-commit-statistics contributor-list % base-path) contributor-names)
        ]
       ]
      [:h1 "Rankings"]
      (create-contributors-ranking-html (get-in analysis [:contributors-statistics :rankings]))
      )))

(defn create-file-change-statistics
  "Creates file change statistics page(s)"
  [base-path file-change-statistics]
  (let [
        site-name (string/join [base-path "file-change-statistics.html"])
        per-file-statistics (:per-file file-change-statistics)
        deleted-files (:deleted-files file-change-statistics)
        still-existing-files (get-in file-change-statistics [:creation-statistics :still-existing])
        files-ordered-by-creation-date (get-in file-change-statistics [:creation-statistics :ordered-by-creation-date])
        edit-count-ranking (get-in file-change-statistics [:edit-statistics :edit-count-ranking])
        file-change-statistics-html
        (html
          [:h2 "Most edited files"]
          [:table {:class "table table-striped"}
           [:thead
            [:tr
             [:th {:scope "col"} "Filename"]
             [:th {:scope "col"} "Nr of edits"]
             ]
            ]
           [:tbody
            (map #(vector
                    :tr
                    [:td (first %)]
                    [:td (second %)]
                    ) edit-count-ranking)
            ]
           ]
          [:h2 "Files ordered by creation date"]
          [:ul (map #(vector :li %) files-ordered-by-creation-date)]
          [:h2 "Files still existing in Repo"]
          [:ul (map #(vector :li %) still-existing-files)]
          [:h2 "Deleted Files"]
          [:ul (map #(vector :li %) deleted-files)]
          [:h2 "File edit statistics"]
          [:table {:class "table table-striped"}
           [:thead
            [:tr
             [:th {:scope "col"} "Filename"]
             [:th {:scope "col"} "Nr of edits"]
             ]
            ]
           [:tbody
            (map #(vector
                    :tr
                    [:td (first %)]
                    [:td (count (:edits (second %)))]
                    ) per-file-statistics)
            ]
           ]
          )]
    (pprint files-ordered-by-creation-date)
    (create-site site-name "File change statistics" file-change-statistics-html)
    (html
      [:a {:href site-name} "File change statistics"]
      )
    ))


(defn create-commit-statistics
  "Creates commit statistics HTML and subpages. Returns the created HTML"
  [analysis base-path]
  (let [
        commit-list-html (create-commit-list-html (:commits (:commit-statistics analysis)))
        self-commit-list-html (create-commit-list-html (get-in analysis [:commit-statistics :self-committed :commits]))
        different-committer-list-html (create-commit-list-html (get-in analysis [:commit-statistics :committed-by-different-dev :commits]))
        all-commits-filename (string/join [base-path "all-commits-list.html"])
        self-commits-filename (string/join [base-path "self-committed-list.html"])
        different-committer-filename (string/join [base-path "different-committer-list.html"])
        pie-chart-dataset (string/join "," [
                                            (get-in analysis [:commit-statistics :self-committed :count])
                                            (get-in analysis [:commit-statistics :committed-by-different-dev :count])])
        line-chart-labels (string/join "," (map #(string/join ["\"" (first %) "\""]) (get-in analysis [:commit-statistics :time-distribution])))
        line-chart-data-authored (string/join "," (map #(:authored (second %)) (get-in analysis [:commit-statistics :time-distribution])))
        line-chart-data-committed (string/join "," (map #(:committed (second %)) (get-in analysis [:commit-statistics :time-distribution])))
        file-change-statistics-html (create-file-change-statistics base-path (:file-change-statistics analysis))
        ]
    (create-site all-commits-filename "All commits" commit-list-html)
    (create-site self-commits-filename "Self committed commits" self-commit-list-html)
    (create-site different-committer-filename "Commits where committer and author are different" different-committer-list-html)
    (html
      [:h1 "Commit analysis"]
      [:canvas {:id "commit-merge-chart" :width "770px" :height "385px"}]
      [:script "new Chart('commit-merge-chart',
      {'type':'pie','data':{'labels':['Self committed','Committer and author are different'],
      'datasets':[{'label':'Committed / Authored','data': [" pie-chart-dataset "],
      'backgroundColor':['rgb(255, 99, 132)','rgb(54, 162, 235)','rgb(255, 205, 86)']}]},
      'options': {'responsive': false}});
      "]
      [:canvas {:id "commit-timeline-chart" :width "770px" :height "385px"}]
      [:script "new Chart('commit-timeline-chart',{'type':'line','data':
      {'labels':[" line-chart-labels "],
      'datasets':[
      {'label':'Authored commits','data':[" line-chart-data-authored "],'fill':false,'borderColor':'rgb(75, 192, 192)','lineTension':0.1},
      {'label':'Committed commits','data':[" line-chart-data-committed "],'fill':false,'borderColor':'rgb(255, 165, 0)','lineTension':0.1}
      ]},'options':{'responsive': false}});"
       ]
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
      file-change-statistics-html
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
