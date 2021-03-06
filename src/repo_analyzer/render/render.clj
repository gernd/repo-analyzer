(ns repo-analyzer.render.render
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log]
            [repo-analyzer.util :as util])
  (:import (java.io File))
  (:use [repo-analyzer.render.commits :only (render-commits-startpage-content render-commits-html-files)]
        [repo-analyzer.render.common :only (create-commit-list-html create-site-html)]))

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

(def contributor-authored-commits-filename-suffix "-authored-commits.html")
(defn contributor-authored-commits-filename [contributor-name] (string/join [contributor-name contributor-authored-commits-filename-suffix]))
(defn contributor-authored-commits-url [contributor-name base-path] (string/join [base-path (contributor-authored-commits-filename contributor-name)]))

(def contributor-committed-commits-filename-suffix "-committed-commits.html")
(defn contributor-committed-commits-filename [contributor-name] (string/join [contributor-name contributor-committed-commits-filename-suffix]))
(defn contributor-committed-commits-url [contributor-name base-path] (string/join [base-path (contributor-committed-commits-filename contributor-name)]))

(def contributor-authored-and-committed-commits-filename-suffix "-authored-and-committed-commits.html")
(defn contributor-authored-and-committed-commits-filename [contributor-name] (string/join [contributor-name contributor-authored-and-committed-commits-filename-suffix]))
(defn contributor-authored-and-committed-commits-url [contributor-name base-path] (string/join [base-path (contributor-authored-and-committed-commits-filename contributor-name)]))

(def contributors-overview-filename "overview.html")
(defn contributors-overview-url [base-path] (string/join [base-path contributors-overview-filename]))

(defn get-contributor-site-name
  [contributor-id]
  (string/join [contributor-id ".html"]))

(defn get-contributor-site-url
  "Computes the URL for the contributor page via the given base-path and ID"
  [base-path id]
  (string/join [base-path (get-contributor-site-name id)]))

(defn create-contributor-commit-statistics
  [contributor-statistics contributor-name base-path]
  (let [authored-commits-list-site-name (contributor-authored-commits-url contributor-name base-path)
        contributor-id (get-in contributor-statistics [contributor-name :id])
        authored-commits-count (get-in contributor-statistics [contributor-name :authored-commits :count])
        committed-commits-count (get-in contributor-statistics [contributor-name :committed-commits :count])
        committed-commits-list-site-name (contributor-committed-commits-url contributor-name base-path)
        authored-and-committed-commits-count (get-in contributor-statistics [contributor-name :authored-and-committed-commits :count])
        authored-and-committed-commits-list-site-name (contributor-authored-and-committed-commits-url contributor-name base-path)]
    (html
     [:tr
      [:td (create-gravatar-html (:email (get contributor-statistics contributor-name)))]
      [:td [:a {:href (get-contributor-site-url base-path contributor-id)} contributor-name]]
      [:td (:email (get contributor-statistics contributor-name))]
      [:td
       [:a {:href authored-commits-list-site-name} authored-commits-count]]
      [:td
       [:a {:href committed-commits-list-site-name} committed-commits-count]]
      [:td
       [:a {:href authored-and-committed-commits-list-site-name} authored-and-committed-commits-count]]])))

(defn create-contributors-statistics-site
  "Creates HTML for contributors statistics and creates subpages"
  [analysis base-path]
  (let [contributor-list (get-in analysis [:contributors-statistics :single-contributor-statistics])
        contributor-names (keys contributor-list)
        site-content (html
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
                      (create-contributors-ranking-html (get-in analysis [:contributors-statistics :rankings])))]
    (create-site-html "Contributor statistics" site-content)))

(defn render-contributor-site [contributor-statistics contributor-name]
  (let [authored-commits-list-html (create-commit-list-html (get-in contributor-statistics [contributor-name :authored-commits :commits]))
        authored-commits-list-site-html (create-site-html (string/join ["Commits authored by " contributor-name]) authored-commits-list-html)
        authored-commits-list-site-name (contributor-authored-commits-filename contributor-name)
        committed-commits-list-html (create-commit-list-html (get-in contributor-statistics [contributor-name :committed-commits :commits]))
        committed-commits-list-site-name (contributor-committed-commits-filename contributor-name)
        committed-commits-site-html (create-site-html (string/join ["Commits committed by " contributor-name]) committed-commits-list-html)
        authored-and-committed-commits-list-html (create-commit-list-html (get-in contributor-statistics [contributor-name :authored-and-committed-commits :commits]))
        authored-and-committed-commits-list-site-name (contributor-authored-and-committed-commits-filename contributor-name)
        authored-and-committed-commits-site-html (create-site-html (string/join ["Commits authored and committed by " contributor-name]) authored-and-committed-commits-list-html)]
    (vector [authored-commits-list-site-name authored-commits-list-site-html]
            [committed-commits-list-site-name committed-commits-site-html]
            [authored-and-committed-commits-list-site-name authored-and-committed-commits-site-html])))

(defn render-contributor-individual-site
  "Renders HTML for a single contributor site"
  [single-contributor-analytics]
  (let [name (:name single-contributor-analytics)
        gravatar-html (create-gravatar-html (:email single-contributor-analytics))
        committed-commits-html (create-commit-list-html (get-in single-contributor-analytics [:committed-commits :commits]))
        authored-commits-html (create-commit-list-html (get-in single-contributor-analytics [:authored-commits :commits]))
        authored-and-committed-commits-html (create-commit-list-html (get-in single-contributor-analytics [:authored-and-committed-commits :commits]))
        site-content (html
                      [:h1 name]
                      [:div gravatar-html]
                      [:h1 "Committed commits"]
                      committed-commits-html
                      [:h1 "Authored commits"]
                      authored-commits-html
                      [:h1 "Authored and committed commits"]
                      authored-and-committed-commits-html)
        site-html (create-site-html name site-content)]
    site-html))

(defn render-contributors-individual-sites
  [contributors-statistics]
  (mapcat #(let [id (:id (second %))
                 url (get-contributor-site-name id)
                 site-content (render-contributor-individual-site (second %))]
             (vector [url site-content])) contributors-statistics))

(defn render-contributor-sites
  [analysis base-path]
  (let [contributor-list (get-in analysis [:contributors-statistics :single-contributor-statistics])
        contributor-names (keys contributor-list)
        contributor-commit-sites (mapcat #(render-contributor-site contributor-list %) contributor-names)
        overview-site [[contributors-overview-filename (create-contributors-statistics-site analysis base-path)]]
        contributors-sites (render-contributors-individual-sites (get-in analysis [:contributors-statistics :single-contributor-statistics]))
        all-sites (apply concat [contributor-commit-sites contributors-sites overview-site])]
    {:path    base-path
     :files   all-sites
     :folders []}))

(defn create-contributors-startpage-html [analysis base-path]
  (let [top5-authors (take 5 (:authored-commits-ranking (get-in analysis [:contributors-statistics :rankings])))]
    (html
     [:h2 "Most authored"]
     [:table {:class "table table-striped"}
      [:thead
       [:tr
        [:th {:scope "col"} "Author name"]
        [:th {:scope "col"} "Nr of commits"]]]

      [:tbody
       (map #(vector
              :tr
              [:td [:a {:href (get-contributor-site-url base-path (:id %))} (:name %)]]
              [:td (:authored-commits-count %)]) top5-authors)]]
     [:a {:href (contributors-overview-url base-path)} "All contributor statistics"])))

(def file-change-statistics-filename "file-change-statistics.html")
(defn file-change-statistics-full-path [base-path] (string/join [base-path file-change-statistics-filename]))

(defn create-file-change-statistics-startpage-html [file-change-statistics base-path]
  (let [most-edited-files (take 5 (get-in file-change-statistics [:edit-statistics :edit-count-ranking]))
        most-recent-files (take-last 5 (get-in file-change-statistics [:creation-statistics :ordered-by-creation-date]))]
    (html
     [:div.container
      [:div.row
       [:div.col
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
                 [:td (second %)]) most-edited-files)]]]
       [:div.col
        [:h2 "Newest files"]
        [:table {:class "table table-striped"}
         [:thead
          [:tr
           [:th {:scope "col"} "Filename"]]]
         [:tbody
          (map #(vector :tr [:td %]) most-recent-files)]]]]]
     [:p [:a {:href (file-change-statistics-full-path base-path)} "All file statistics"]])))

(defn create-file-change-statistics-html
  "Creates file change statistics page(s)"
  [file-change-statistics]
  (let [per-file-statistics (:per-file file-change-statistics)
        deleted-files (:deleted-files file-change-statistics)
        still-existing-files (get-in file-change-statistics [:creation-statistics :still-existing])
        files-ordered-by-creation-date (get-in file-change-statistics [:creation-statistics :ordered-by-creation-date])
        edit-count-ranking (get-in file-change-statistics [:edit-statistics :edit-count-ranking])
        file-change-statistics-site-content
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
    (create-site-html "File change statistics" file-change-statistics-site-content)))

(defn render-file-change-statistics-sites [file-change-statistics base-path]
  {:path    base-path
   :files   [[file-change-statistics-filename (create-file-change-statistics-html file-change-statistics)]]
   :folders []})

(defn create-meta-data-html
  "Creates HTML for the analysis' meta data"
  [analysis]
  (html
   [:h1 "Repository analysis for " (get-in analysis [:meta-data :repo-name]) " created " (get-in analysis [:meta-data :creation-date])]))

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
  [collab-statistics]
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

(defn copy-js-files
  [target-path]
  (doseq [filename ["viz.js" "full.render.js"]]
    (try
      (io/copy
       (io/file (.getFile (io/resource (string/join ["js/" filename]))))
       (io/file (string/join [target-path filename])))
      (catch Exception e (log/error "Exception during copying file: " (.getMessage e) (.toString e))))))

(defn write-html-report-to-disc
  [rendered-report-folder]
  (let [folder-name (:path rendered-report-folder)
        files (:files rendered-report-folder)
        folders (:folders rendered-report-folder)]
    (log/info "Creating folder for HTML output:" folder-name)
    (.mkdirs (File. folder-name))
    (doseq [file-entry files]
      (let [file-name (first file-entry)
            full-file-path (string/join [folder-name file-name])
            file-content (second file-entry)]
        (log/info "Creating file" full-file-path)
        (spit full-file-path file-content)))

    (log/info "All files created for folder" folder-name)
    (doseq [subfolder folders] (write-html-report-to-disc subfolder))))

(defn render-html-report
  [analysis base-path]
  (let [commits-folder (string/join [base-path "commits/"])
        file-change-statistics-folder (string/join [base-path "file-change-statistics/"])
        contributors-folder (string/join [base-path "contributors/"])
        meta-data-html (create-meta-data-html analysis)
        commit-statistics-html (render-commits-startpage-content analysis commits-folder)
        file-change-statistics-html (create-file-change-statistics-startpage-html (:file-change-statistics analysis) file-change-statistics-folder)
        contributors-html (create-contributors-startpage-html analysis contributors-folder)
        collaboration-html (create-collaboration-statistics (:collaboration-statistics analysis))
        startpage-content (string/join [meta-data-html commit-statistics-html file-change-statistics-html contributors-html collaboration-html])
        startpage-html (create-site-html "Repository Analysis" startpage-content)]
    {:path    base-path :files [["index.html" startpage-html]]
     :folders [(render-commits-html-files analysis commits-folder)
               (render-file-change-statistics-sites (:file-change-statistics analysis) file-change-statistics-folder)
               (render-contributor-sites analysis contributors-folder)]}))

(defn create-html-report
  "Renders the repository analysis as HTML and saves it in the given directory"
  [repo-analysis output-dir]
  (let [rendered-report (render-html-report repo-analysis output-dir)
        js-folder (string/join [output-dir "js/"])]
    (write-html-report-to-disc rendered-report)
    (.mkdirs (File. js-folder))
    (copy-js-files js-folder)))

(defn render-analysis-pprint
  "Renders the repository analysis by just dumping it on the command line"
  [repo-analysis]
  (pprint repo-analysis))
