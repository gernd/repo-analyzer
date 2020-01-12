(ns repo-analyzer.render.common
  (:require [clojure.tools.logging :as log]))

(use 'hiccup.core)

(defn create-commit-list-html
  "Creates HTML for a list of commits"
  [list-of-commits]
  (html
   [:table {:class "table table-striped"}
    [:thead
     [:tr
      [:th {:scope "col"} "Message"]
      [:th {:scope "col"} "Author Name"]
      [:th {:scope "col"} "Author Email"]
      [:th {:scope "col"} "Authored at"]
      [:th {:scope "col"} "Committer Name"]
      [:th {:scope "col"} "Committer Email"]
      [:th {:scope "col"} "Committed at"]]]

    [:tbody
     (map #(vector
            :tr
            [:td (:msg %)]
            [:td (get-in % [:author :name])]
            [:td (get-in % [:author :email])]
            [:td (get-in % [:author :date])]
            [:td (get-in % [:committer :name])]
            [:td (get-in % [:committer :email])]
            [:td (get-in % [:committer :date])]) list-of-commits)]]))

(defn create-site
  "Creates a HTML site with basic header / footer and the given content and saves it as a file"
  [filename site-title html-content]
  (let [site-html (html
                   [:head
                    [:title site-title]
                    [:meta {:charset "utf-8"}]
                    [:meta {:name "viewport", :content "width=device-width, initial-scale=1, shrink-to-fit=no"}]
                    [:link {:rel         "stylesheet"
                            :href        "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
                            :integrity   "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
                            :crossorigin "anonymous"}]
                    [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.8.0/Chart.bundle.min.js"}]]
                    ; TODO include JS if needed for bootstrap
                    ;<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
                    ;<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
                    ;<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>
                   [:body html-content])]

    (log/info "Creating " filename)
    (spit filename site-html)))

(defn create-site-html
  "Creates a HTML site with basic header / footer and the given content"
  [site-title html-content]
  (html
   [:head
    [:title site-title]
    [:meta {:charset "utf-8"}]
    [:meta {:name "viewport", :content "width=device-width, initial-scale=1, shrink-to-fit=no"}]
    [:link {:rel         "stylesheet"
            :href        "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
            :integrity   "sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T"
            :crossorigin "anonymous"}]
    [:script {:src "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.8.0/Chart.bundle.min.js"}]]
    ; TODO include JS if needed for bootstrap
    ;<script src="https://code.jquery.com/jquery-3.3.1.slim.min.js" integrity="sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" crossorigin="anonymous"></script>
    ;<script src="https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js" integrity="sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1" crossorigin="anonymous"></script>
    ;<script src="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js" integrity="sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM" crossorigin="anonymous"></script>
   [:body html-content]))
