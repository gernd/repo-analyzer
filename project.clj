(defproject repo-analyzer "0.5.0-SNAPSHOT"
  :description "CLI Analysis Tool for Git repositories"
  :url "https://github.com/gernd/repo-analyzer"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"],
                 [clj-jgit "1.0.0-beta2":exclusions [org.slf4j/slf4j-api]],
                 [org.clojure/tools.trace "0.7.10"]
                 [hiccup "1.0.5"]
                 [org.clojure/tools.logging "1.0.0"]
                 [ch.qos.logback/logback-classic "1.2.3"]
                 [clj-kondo "RELEASE"]
                 ]
  :aliases {"clj-kondo" ["run" "-m" "clj-kondo.main"]}
  :plugins [
            [lein-cloverage "1.1.2"]
            [lein-cljfmt "0.6.6"]
            [lein-codox "0.10.7"]
            [lein-ancient "0.6.15"]
            ]
  :main ^:skip-aot repo-analyzer.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
