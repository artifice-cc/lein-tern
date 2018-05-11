(defproject cc.artifice/lein-tern "0.5.5"
  :description "Migrations as data"
  :url "http://github.com/artifice-cc/lein-tern"
  :license {:name "MIT"
            :url "http://opensource.org/licenses/MIT"}
  :scm  {:name "git"
         :url "https://github.com/artifice-cc/lein-tern"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-time "0.11.0"]
                 [org.clojure/java.jdbc "0.4.2"]
                 [postgresql "9.3-1102.jdbc41"]
                 [java-jdbc/dsl "0.1.3"]]
  :profiles {:dev {:dependencies [[expectations "2.1.4"]]
                   :plugins [[lein-autoexpect "1.7.0"]
                             [lein-expectations "0.0.8"]]}}
  :eval-in-leiningen true)
