(ns repo-analyzer.render.commits
  (:require [clojure.string :as string])
  (:use [repo-analyzer.render.common :only (create-commit-list-html create-site create-site-html)]))

(use 'hiccup.core)
(use 'clojure.tools.trace)

(defn create-commit-length-statistics-html
  [commit-length-statistics]
  (let [top5-longest-messages (take 5 commit-length-statistics)
        top5-shortest-messages (take-last 5 commit-length-statistics)]
    (html
     [:h2 "Commit message length"]
     [:h3 "Top 5 longest commit messages"]
     [:ol
      (map
       #(vector :li (string/join [(:author %) ": " (:message %) " (" (:length %) " characters)"])) top5-longest-messages)]

     [:h3 "Top 5 shortest commit messages"]
     [:ol
      (map
       #(vector :li (string/join [(:author %) ": " (:message %) " (" (:length %) " characters)"])) top5-shortest-messages)])))

(defn create-commit-count-line-chart
  "Creates a line chart for the provided commit count statistics"
  [commit-count-statistics id]
  (let [line-chart-labels (string/join "," (map #(string/join ["\"" (first %) "\""]) commit-count-statistics))
        line-chart-data-authored (string/join "," (map #(:authored (second %)) commit-count-statistics))
        line-chart-data-committed (string/join "," (map #(:committed (second %)) commit-count-statistics))]

    (html
     [:canvas {:id id :width "770px" :height "385px"}]
     [:script "new Chart('" id "',{'type':'line','data':
      {'labels':[" line-chart-labels "],
      'datasets':[
      {'label':'Authored commits','data':[" line-chart-data-authored "],'fill':false,'borderColor':'rgb(75, 192, 192)','lineTension':0.1},
      {'label':'Committed commits','data':[" line-chart-data-committed "],'fill':false,'borderColor':'rgb(255, 165, 0)','lineTension':0.1}
      ]},'options':{'responsive': false}});"])))


(def all-commits-filename "all-commits.html")
(def self-committed-commits-filename "self-committed-commits.html")
(def different-committer-filename "commits-with-different-committer.html")
(def late-commits-filename "late-commits.html")
(def early-commits-filename "early-commits.html")
(def working-day-commits-filename "working-day-commits.html")
(def weekend-commits-filename "weekend-commits.html")

(defn self-committed-commits-url [base-path] (string/join [base-path self-committed-commits-filename]))
(defn all-commits-url [base-path] (string/join [base-path all-commits-filename]))
(defn different-committer-url [base-path] (string/join [base-path different-committer-filename]))
(defn late-commits-url [base-path] (string/join [base-path late-commits-filename]))
(defn early-commits-url [base-path] (string/join [base-path early-commits-filename]))
(defn working-day-commits-url [base-path] (string/join [base-path working-day-commits-filename]))
(defn weekend-commits-url [base-path] (string/join [base-path weekend-commits-filename]))

(defn render-commits-html-files [analysis base-path]
  (let [all-commits-content (create-commit-list-html (:commits (:commit-statistics analysis)))
        all-commits-html (create-site-html "All commits" all-commits-content)
        self-commit-list-content (create-commit-list-html (get-in analysis [:commit-statistics :percentages :committer-vs-author :self-committed :commits]))
        self-commit-list-html (create-site-html "Self committed commits" self-commit-list-content)
        different-committer-list-content (create-commit-list-html (get-in analysis [:commit-statistics :percentages :committer-vs-author :committed-by-different-dev :commits]))
        different-committer-list-html (create-site-html "Commits with different author/comitter" different-committer-list-content)
        late-commits-list-content (create-commit-list-html (get-in analysis [:commit-statistics :percentages :time-of-day :late-commits :commits]))
        late-commits-list-html (create-site-html "Late commits" late-commits-list-content)
        early-commits-list-content (create-commit-list-html (get-in analysis [:commit-statistics :percentages :time-of-day :early-commits :commits]))
        early-commits-list-html (create-site-html "Early commits" early-commits-list-content)
        workdays-commits-list-content (create-commit-list-html (get-in analysis [:commit-statistics :percentages :day-of-week :commits-on-working-days :commits]))
        workdays-commits-html (create-site-html "Commits on workdays" workdays-commits-list-content)
        weekend-commits-list-content (create-commit-list-html (get-in analysis [:commit-statistics :percentages :day-of-week :commits-on-weekend :commits]))
        weekend-commits-html (create-site-html "Commits on weekends" weekend-commits-list-content)
        ]
    {:path    base-path
     :files   [[all-commits-filename all-commits-html]
               [self-committed-commits-filename self-commit-list-html]
               [different-committer-filename different-committer-list-html]
               [late-commits-filename late-commits-list-html]
               [early-commits-filename early-commits-list-html]
               [working-day-commits-filename workdays-commits-html]
               [weekend-commits-filename weekend-commits-html]]
     :folders []}))

(defn create-commit-statistics-html
  "Creates commit statistics HTML for the start page"
  [analysis base-path]
  (let [workdays-commits-count (get-in analysis [:commit-statistics :percentages :day-of-week :commits-on-working-days :count])
        weekend-commits-count (get-in analysis [:commit-statistics :percentages :day-of-week :commits-on-weekend :count])
        self-committed-count (get-in analysis [:commit-statistics :percentages :committer-vs-author :self-committed :count])
        different-committer-count (get-in analysis [:commit-statistics :percentages :committer-vs-author :committed-by-different-dev :count])
        pie-chart-dataset-committer-vs-author (string/join "," [self-committed-count different-committer-count])
        pie-chart-dataset-workingday-vs-weekend (string/join "," [workdays-commits-count weekend-commits-count])
        commit-length-statistics-html (create-commit-length-statistics-html (get-in analysis [:commit-statistics :commit-message-length-ranking]))
        total-commit-count (get-in analysis [:commit-statistics :count-statistics :total-count])]
    (html
     [:h1 "Commit analysis"]
     [:p "Commits analyzed: " total-commit-count
      [:a {:href (all-commits-url base-path)} " See list of all commits"]]
     [:h2 "Percentages"]
     [:canvas {:id "commit-merge-chart" :width "770px" :height "385px"}]
     [:script "new Chart('commit-merge-chart',
      {'type':'pie','data':{'labels':['Self committed','Committer and author are different'],
      'datasets':[{'label':'Committed / Authored','data': [" pie-chart-dataset-committer-vs-author "],
      'backgroundColor':['rgb(255, 99, 132)','rgb(54, 162, 235)','rgb(255, 205, 86)']}]},
      'options': {'responsive': false}});
      "]
     [:p "Self committed commits: " self-committed-count "/"
      total-commit-count "(" (get-in analysis [:commit-statistics :percentages :committer-vs-author :self-committed :percentage]) "%)"
      [:a {:href (self-committed-commits-url base-path)} " See list of all self-committed commits"]]
     [:p "Committer and author are different: " different-committer-count "/"
      total-commit-count "(" (get-in analysis [:commit-statistics :percentages :committer-vs-author :committed-by-different-dev :percentage]) "%)"
      [:a {:href (different-committer-url base-path)} " See list of all commits where author and committer are different"]]
     [:h2 "Commit time distribution"]
     [:p [:a {:href (late-commits-url base-path)} " Late commits"]]
     [:a {:href (early-commits-url base-path)} " Early commits"]
     [:h2 "Commit day of week distribution"]
     [:canvas {:id "commit-day-chart" :width "770px" :height "385px"}]
     [:script "new Chart('commit-day-chart',
      {'type':'pie','data':{'labels':['Authored on working day','Authored on weekend'],
      'datasets':[{'label':'Working day / Weekend','data': [" pie-chart-dataset-workingday-vs-weekend "],
      'backgroundColor':['rgb(255, 99, 132)','rgb(54, 162, 235)','rgb(255, 205, 86)']}]},
      'options': {'responsive': false}});
      "]
     [:p [:a {:href (working-day-commits-url base-path)} " Commits authored on working days (" workdays-commits-count ")"]]
     [:p [:a {:href (weekend-commits-url base-path)} " Commits authored on the weekend (" weekend-commits-count ")"]]
     [:h2 "Nr of commits by day"]
     (create-commit-count-line-chart (get-in analysis [:commit-statistics :count-statistics :count-by-day]) "commits-by-day")
     [:h2 "Nr of commits by week"]
     (create-commit-count-line-chart (get-in analysis [:commit-statistics :count-statistics :count-by-week]) "commits-by-week")
     [:h2 "Nr of commits by month"]
     (create-commit-count-line-chart (get-in analysis [:commit-statistics :count-statistics :count-by-month]) "commits-by-month")
     [:h2 "Nr of commits by year"]
     (create-commit-count-line-chart (get-in analysis [:commit-statistics :count-statistics :count-by-year]) "commits-by-year")
     commit-length-statistics-html)))
