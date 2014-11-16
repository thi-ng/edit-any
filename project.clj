(defproject ea "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [compojure "1.2.0"]
                 [ring/ring-defaults "0.1.2"]
                 [liberator "0.12.2"]
                 [thi.ng/trio "0.1.0-SNAPSHOT"]
                 [clj-time "0.8.0"]
                 [hiccup "1.0.5"]
                 [markdown-clj "0.9.57" :exclusions [org.clojure/clojure]]]
  :plugins [[lein-ring "0.8.13"]]
  :ring {:handler ea.core.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}})
