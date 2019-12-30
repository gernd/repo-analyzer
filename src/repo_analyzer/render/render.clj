(ns repo-analyzer.render.render
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [repo-analyzer.util :as util])
  (:import (java.io File))
  (:use [repo-analyzer.render.commits :only (create-commit-statistics)]
        [repo-analyzer.render.common :only (create-commit-list-html create-site)]))

(use 'clojure.pprint)
(use 'clojure.tools.trace)
(use 'hiccup.core)

(defn create-gravatar-html
  [email]
  (let [gravatar-url (string/join
                      ["https://www.gravatar.com/avatar/"
                       (-> email
                           string/trim
                           string/lower-case
                           util/md5)])]
    (html
     [:img {:src gravatar-url}])))

(defn create-contributors-ranking-html
  [contributor-rankings]
  (let [top-5-authored (take 5 (:authored-commits-ranking contributor-rankings))
        top-5-committed (take 5 (:committed-commits-ranking contributor-rankings))
        top-5-authored-and-committed (take 5 (:authored-and-committed-commits-ranking contributor-rankings))]
    (html
     [:h2 "Authored commits"]
     [:ol
      (map
       #(vector :li (string/join [(:name %) " - " (:authored-commits-count %)])) top-5-authored)]

     [:h2 "Committed commits"]
     [:ol
      (map
       #(vector :li (string/join [(:name %) " - " (:committed-commits-count %)])) top-5-committed)]

     [:h2 "Authored and committed commits"]
     [:ol
      (map
       #(vector :li (string/join [(:name %) " - " (:authored-and-committed-commits-count %)])) top-5-authored-and-committed)])))

(defn create-contributor-commit-statistics
  [contributor-statistics contributor-name base-path]
  (let [authored-commits-list-html (create-commit-list-html (get-in contributor-statistics [contributor-name :authored-commits :commits]))
        authored-commits-list-site-name (string/join [base-path contributor-name "-authored-commits.html"])
        authored-commits-count (get-in contributor-statistics [contributor-name :authored-commits :count])
        committed-commits-list-html (create-commit-list-html (get-in contributor-statistics [contributor-name :committed-commits :commits]))
        committed-commits-count (get-in contributor-statistics [contributor-name :committed-commits :count])
        committed-commits-list-site-name (string/join [base-path contributor-name "-committed-commits.html"])
        authored-and-committed-commits-list-html (create-commit-list-html (get-in contributor-statistics [contributor-name :authored-and-committed-commits :commits]))
        authored-and-committed-commits-count (get-in contributor-statistics [contributor-name :authored-and-committed-commits :count])
        authored-and-committed-commits-list-site-name (string/join [base-path contributor-name "-authored-and-committed-commits.html"])]
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
       [:a {:href authored-commits-list-site-name} authored-commits-count]]
      [:td
       [:a {:href committed-commits-list-site-name} committed-commits-count]]
      [:td
       [:a {:href authored-and-committed-commits-list-site-name} authored-and-committed-commits-count]]])))

(defn create-contributors-statistics
  "Creates HTML for contributors statistics and creates subpages"
  [analysis base-path]
  (let [contributor-list (get-in analysis [:contributors-statistics :single-contributor-statistics])
        contributor-names (keys contributor-list)]
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
        [:th {:scope "col"} "# of authored and committed commits"]]]

      [:tbody
       (map #(create-contributor-commit-statistics contributor-list % base-path) contributor-names)]]

     [:h1 "Rankings"]
     (create-contributors-ranking-html (get-in analysis [:contributors-statistics :rankings])))))

(defn create-file-change-statistics
  "Creates file change statistics page(s)"
  [base-path file-change-statistics]
  (let [site-name (string/join [base-path "file-change-statistics.html"])
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
            [:th {:scope "col"} "Nr of edits"]]]

          [:tbody
           (map #(vector
                  :tr
                  [:td (first %)]
                  [:td (second %)]) edit-count-ranking)]]

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
            [:th {:scope "col"} "Nr of edits"]]]

          [:tbody
           (map #(vector
                  :tr
                  [:td (first %)]
                  [:td (count (:edits (second %)))]) per-file-statistics)]])]

    (create-site site-name "File change statistics" file-change-statistics-html)
    (html
     [:a {:href site-name} "File change statistics"])))

(defn create-meta-data-html
  "Creates HTML for the analysis' meta data"
  [analysis]
  (html
   [:h1 "Analysis information"]
   [:p "Repository: " (get-in analysis [:meta-data :repo-name])]
   [:p "Created: " (get-in analysis [:meta-data :creation-date])]))

(defn create-committer-graph
  [collab-statistics]
  (string/join (apply concat
                      (map #(let [normalize-email-address (fn [email-address]
                                                            (string/replace (string/replace email-address "." "_dot_") "@" "_at_"))
                                  committer-name (normalize-email-address (first %))
                                  author-list (second %)]
                              (map (fn [author-name] (string/join [committer-name " -> " (normalize-email-address author-name) " "])) author-list))
                           collab-statistics))))

(defn create-collaboration-statistics
  "Creates collaboration statistic HTML and creates corresponding pages"
  [collab-statistics base-path]
  (html
   [:h1 "Collaboration statistics"]
   [:script {:src "js/viz.js"}]
   [:script {:src "js/full.render.js"}]
   [:div {:id "collab-graph"}]
   [:script
    "var viz = new Viz();"
    "viz.renderSVGElement('digraph {"
    (create-committer-graph collab-statistics)
    "}')
      .then(function(element) {document.getElementById('collab-graph').appendChild(element);})
       .catch(error => {
          // Create a new Viz instance (@see Caveats page for more info)
             viz = new Viz();

          // Possibly display the error
             console.error(error);
          });"]))

(defn create-analysis-html-report
  "Creates the index site for the given analysis including all subpages"
  [analysis base-path]
  (let [commit-statistics-html (create-commit-statistics analysis base-path)
        file-change-statistics-html (create-file-change-statistics base-path (:file-change-statistics analysis))
        contributors-html (create-contributors-statistics analysis base-path)
        meta-data-html (create-meta-data-html analysis)
        collaboration-html (create-collaboration-statistics (:collaboration-statistics analysis) base-path)
        index-site-html (string/join
                         [meta-data-html
                          commit-statistics-html
                          file-change-statistics-html
                          contributors-html
                          collaboration-html])
        index-site-name (string/join [base-path "index.html"])
        repo-name (get-in analysis [:meta-data :repo-name])]
    (create-site index-site-name (string/join ["Git repo analysis for " repo-name]) index-site-html)))

(defn copy-js-files
  [target-path]
  (doseq [filename ["viz.js" "full.render.js"]]
    (try
      (io/copy
       (io/file (.getFile (io/resource (string/join ["js/" filename]))))
       (io/file (string/join [target-path filename])))
      (catch Exception e (log/error "Exception during copying file: " (.getMessage e) (.toString e))))))

(defn render-analysis-html
  "Renders the repository analysis as HTML"
  [repo-analysis output-dir]
  (let [js-folder (string/join [output-dir "js/"])]
    (log/info "Generating HTML report")
    (log/info "Creating folder for HTML output:" output-dir)
    (.mkdirs (File. output-dir))
    (log/info "Creating folder for js files:" js-folder)
    (.mkdirs (File. js-folder))
    (copy-js-files js-folder)
    (create-analysis-html-report repo-analysis output-dir)))

(defn render-analysis-pprint
  "Renders the repository analysis by just dumping it on the command line"
  [repo-analysis]
  (pprint repo-analysis))