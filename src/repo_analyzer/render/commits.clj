(ns repo-analyzer.render.commits
  (:require [clojure.string :as string])
  (:use [repo-analyzer.render.common :only (create-commit-list-html create-site)]))

(use 'hiccup.core)

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

(defn create-commit-statistics
  "Creates commit statistics HTML and subpages. Returns the created HTML"
  [analysis base-path]
  (let [commit-list-html (create-commit-list-html (:commits (:commit-statistics analysis)))
        self-commit-list-html (create-commit-list-html (get-in analysis [:commit-statistics :percentages :committer-vs-author :self-committed :commits]))
        different-committer-list-html (create-commit-list-html (get-in analysis [:commit-statistics :percentages :committer-vs-author :committed-by-different-dev :commits]))
        all-commits-filename (string/join [base-path "all-commits-list.html"])
        self-commits-filename (string/join [base-path "self-committed-list.html"])
        different-committer-filename (string/join [base-path "different-committer-list.html"])
        late-commits-filename (string/join [base-path "late-commits.html"])
        late-commits-html (create-commit-list-html (get-in analysis [:commit-statistics :percentages :time-of-day :late-commits :commits]))
        early-commits-filename (string/join [base-path "early-commits.html"])
        workdays-commits-filename (string/join [base-path "workdays-commits.html"])
        workdays-commits-html (create-commit-list-html (get-in analysis [:commit-statistics :percentages :day-of-week :commits-on-working-days :commits]))
        workdays-commits-count (get-in analysis [:commit-statistics :percentages :day-of-week :commits-on-working-days :count])
        weekend-commits-filename (string/join [base-path "weekend-commits.html"])
        weekend-commits-html (create-commit-list-html (get-in analysis [:commit-statistics :percentages :day-of-week :commits-on-weekend :commits]))
        weekend-commits-count (get-in analysis [:commit-statistics :percentages :day-of-week :commits-on-weekend :count])
        early-commits-html (create-commit-list-html (get-in analysis [:commit-statistics :percentages :time-of-day :early-commits :commits]))
        self-committed-count (get-in analysis [:commit-statistics :percentages :committer-vs-author :self-committed :count])
        different-committer-count (get-in analysis [:commit-statistics :percentages :committer-vs-author :committed-by-different-dev :count])
        pie-chart-dataset-committer-vs-author (string/join "," [self-committed-count different-committer-count])
        pie-chart-dataset-workingday-vs-weekend (string/join "," [workdays-commits-count weekend-commits-count])
        commit-length-statistics-html (create-commit-length-statistics-html (get-in analysis [:commit-statistics :commit-message-length-ranking]))
        total-commit-count (get-in analysis [:commit-statistics :count-statistics :total-count])]
    (create-site all-commits-filename "All commits" commit-list-html)
    (create-site self-commits-filename "Self committed commits" self-commit-list-html)
    (create-site different-committer-filename "Commits where committer and author are different" different-committer-list-html)
    (create-site early-commits-filename "Commits authored at night" early-commits-html)
    (create-site late-commits-filename "Commits authored after work" late-commits-html)
    (create-site workdays-commits-filename "Commits authored on working days" workdays-commits-html)
    (create-site weekend-commits-filename "Commits authored on weekend" weekend-commits-html)
    (html
     [:h1 "Commit analysis"]
     [:p "Commits analyzed: " total-commit-count
      [:a {:href "all-commits-list.html"} " See list of all commits"]]
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
      [:a {:href "self-committed-list.html"} " See list of all self-committed commits"]]
     [:p "Committer and author are different: " different-committer-count "/"
      total-commit-count "(" (get-in analysis [:commit-statistics :percentages :committer-vs-author :committed-by-different-dev :percentage]) "%)"
      [:a {:href "different-committer-list.html"} " See list of all commits where author and committer are different"]]
     [:h2 "Commit time distribution"]
     [:p [:a {:href late-commits-filename} " Late commits"]]
     [:a {:href early-commits-filename} " Early commits"]
     [:h2 "Commit day of week distribution"]
     [:canvas {:id "commit-day-chart" :width "770px" :height "385px"}]
     [:script "new Chart('commit-day-chart',
      {'type':'pie','data':{'labels':['Authored on working day','Authored on weekend'],
      'datasets':[{'label':'Working day / Weekend','data': [" pie-chart-dataset-workingday-vs-weekend "],
      'backgroundColor':['rgb(255, 99, 132)','rgb(54, 162, 235)','rgb(255, 205, 86)']}]},
      'options': {'responsive': false}});
      "]
     [:p [:a {:href workdays-commits-filename} " Commits authored on working days (" workdays-commits-count ")"]]
     [:p [:a {:href weekend-commits-filename} " Commits authored on the weekend (" weekend-commits-count ")"]]
     [:h2 "Nr of commits by day"]
     (create-commit-count-line-chart  (get-in analysis [:commit-statistics :count-statistics :count-by-day]) "commits-by-day")
     [:h2 "Nr of commits by week"]
     (create-commit-count-line-chart  (get-in analysis [:commit-statistics :count-statistics :count-by-week]) "commits-by-week")
     [:h2 "Nr of commits by month"]
     (create-commit-count-line-chart  (get-in analysis [:commit-statistics :count-statistics :count-by-month]) "commits-by-month")
     [:h2 "Nr of commits by year"]
     (create-commit-count-line-chart  (get-in analysis [:commit-statistics :count-statistics :count-by-year]) "commits-by-year")
     commit-length-statistics-html)))
