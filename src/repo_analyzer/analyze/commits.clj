(ns repo-analyzer.analyze.commits
  (:require [clojure.tools.logging :as log]))

(defn compute-commit-count-for-time
  "Computes the commit count for the given commits in intervals defined by the provided date format string"
  [commits date-format-string]

  (let [authored-commit-dates (map
                               #(vector (.format (java.text.SimpleDateFormat. date-format-string)
                                                 (get-in % [:author :date])) :authored) commits)
        committed-commit-dates (map
                                #(vector (.format (java.text.SimpleDateFormat. date-format-string)
                                                  (get-in % [:committer :date])) :committed) commits)
        all-commit-dates (concat authored-commit-dates committed-commit-dates)]
    (->> all-commit-dates
         (reduce
          #(let [commit-time-distribution %1
                 date (first %2)
                 commit-type (second %2)]
             (if (contains? commit-time-distribution date)
               (let
                [number-of-authored-commits (get-in commit-time-distribution [date :authored])
                 number-of-committed-commits (get-in commit-time-distribution [date :committed])]
                 (if (= commit-type :authored)
                   (assoc commit-time-distribution date {:authored (inc number-of-authored-commits) :committed number-of-committed-commits})
                   (assoc commit-time-distribution date {:authored number-of-authored-commits :committed (inc number-of-committed-commits)})))

               (if (= commit-type :authored)
                 (assoc commit-time-distribution date {:authored 1 :committed 0})
                 (assoc commit-time-distribution date {:authored 0 :committed 1}))))

          {})
         (sort #(compare (first %1) (first %2))))))

(defn compute-commit-count-by-day
  "Computes the commit count per day for authored and committed commits"
  [commits]
  (compute-commit-count-for-time commits "yyyy/MM/dd"))

(defn compute-commit-count-by-week
  "Computes the commit count per week for authored and committed commits"
  [commits]
  (compute-commit-count-for-time commits "yyyy/ww"))

(defn compute-commit-count-by-month
  "Computes the commit count per month for authored and committed commits"
  [commits]
  (compute-commit-count-for-time commits "yyyy/MM"))

(defn compute-commit-count-by-year
  "Computes the commit count per year for authored and committed commits"
  [commits]
  (compute-commit-count-for-time commits "yyyy"))

(defn compute-commit-daytime-distribution
  "Computes the distribution of the given commits regarding the daytime they were authored"
  [commits]
  (let [commits-with-author-time (map #(let [author-commit-date (get-in % [:author :date])
                                             author-commit-time-formatted (.format (java.text.SimpleDateFormat. "yyyy/MM/dd HH:mm") author-commit-date)
                                             author-commit-hour (Integer/parseInt (.format (java.text.SimpleDateFormat. "HH") author-commit-date))]
                                         (assoc % :author-commit-hour author-commit-hour :author-commit-time author-commit-time-formatted)) commits)
        early-commits (filter #(< (:author-commit-hour %) 8) commits-with-author-time)
        late-commits (filter #(> (:author-commit-hour %) 18) commits-with-author-time)
        regular-commits (filter #(and
                                  (<= (:author-commit-hour %) 18)
                                  (>= (:author-commit-hour %) 8)) commits-with-author-time)]
    {:early-commits   {:commits early-commits :count (count early-commits)}
     :late-commits    {:commits late-commits :count (count late-commits)}
     :regular-commits {:commits regular-commits :count (count regular-commits)}}))

(defn compute-commit-day-of-week-distribution
  "Compute distribution of commits on working days vs. commits authored on the weekend"
  [commits]
  (let [commits-with-day-of-week (map #(let [author-commit-date (get-in % [:author :date])
                                             day-of-week (.format (java.text.SimpleDateFormat. "E" (. java.util.Locale ENGLISH)) author-commit-date)]
                                         (assoc % :day-of-week day-of-week)) commits)
        commits-on-weekend (filter #(or (= "Sat" (:day-of-week %)) (= "Sun" (:day-of-week %))) commits-with-day-of-week)
        commits-on-working-days (filter #(not (or (= "Sat" (:day-of-week %)) (= "Sun" (:day-of-week %)))) commits-with-day-of-week)]
    {:commits-on-weekend {:commits commits-on-weekend :count (count commits-on-weekend)}
     :commits-on-working-days {:commits commits-on-working-days :count (count commits-on-working-days)}}))

(defn compute-commit-message-length-ranking
  "Computes a ranking regarding the length of the commit messages"
  [logs]
  (->> logs
       (map #(hash-map :message (:msg %) :length (count (:msg %)) :author (get-in % [:author :email])))
       (sort-by :length #(compare %2 %1))))

(defn compute-commit-statistics
  "Computes overall commit statistics"
  [logs]
  (future
    (log/info "Computing commit statistics")
    (let [self-committed-commits (filter #(= (:name (:author %)) (:name (:committer %))) logs)
          committed-by-different-dev (filter #(not (= (:name (:author %)) (:name (:committer %)))) logs)
          commit-statistics
          {:commits                       logs
           :count-statistics              {:total-count (count logs)
                                           :count-by-day (compute-commit-count-by-day logs)
                                           :count-by-week (compute-commit-count-by-week logs)
                                           :count-by-month (compute-commit-count-by-month logs)
                                           :count-by-year (compute-commit-count-by-year logs)}
           :percentages                   {:committer-vs-author {:self-committed                {:commits    self-committed-commits
                                                                                                 :count      (count self-committed-commits)
                                                                                                 :percentage (* 100 (double (/ (count self-committed-commits) (count logs))))}

                                                                 :committed-by-different-dev    {:commits    committed-by-different-dev
                                                                                                 :count      (count committed-by-different-dev)
                                                                                                 :percentage (* 100 (double (/ (count committed-by-different-dev) (count logs))))}}
                                           :time-of-day (compute-commit-daytime-distribution logs)
                                           :day-of-week (compute-commit-day-of-week-distribution logs)}

           :commit-message-length-ranking (compute-commit-message-length-ranking logs)}]

      (log/info "Computation of commit statistics finished")
      commit-statistics)))
