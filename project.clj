(defproject ea "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "0.0-3308"]
                 [compojure "1.4.0"]
                 [ring/ring-defaults "0.1.5"]
                 [liberator "0.13"]
                 [thi.ng/trio "0.2.0-SNAPSHOT"]
                 [com.taoensso/timbre "4.0.2"]
                 [clj-time "0.10.0"]
                 ;;[clj-jgit "0.8.8"]
                 [hiccup "1.0.5"]
                 [aleph "0.4.0"]
                 [manifold "0.1.0"]
                 [potemkin "0.4.1"]
                 [com.stuartsierra/component "0.2.3"]
                 [org.clojure/tools.namespace "0.2.10"]
                 [edn-config "0.3"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojars.toxi/markdown-clj "0.9.57" :exclusions [org.clojure/clojure]]]
  :profiles {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                                  [ring/ring-devel "1.4.0"]
                                  [ring/ring-mock "0.2.0"]]
                   :plugins      [[lein-cljsbuild "1.0.6"]]}}
  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src-cljs"]
                        :compiler {:output-to "resources/public/js/app.js"
                                   :optimizations :whitespace}}
                       {:id "release"
                        :source-paths ["src-cljs"]
                        :compiler {:output-to "resources/public/js/app.js"
                                   :optimizations :advanced
                                   :pretty-print false}}]})
