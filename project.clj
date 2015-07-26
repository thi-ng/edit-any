(defproject ea "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "0.0-3308"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [com.stuartsierra/component "0.2.3"]
                 [org.clojure/tools.namespace "0.2.10"]
                 [com.stuartsierra/dependency "0.1.1"]
                 [com.taoensso/timbre "4.0.2"]
                 [ring/ring-defaults "0.1.5"]
                 [ring/ring-devel "1.4.0"]
                 [aleph "0.4.0"]
                 [manifold "0.1.0"]
                 [potemkin "0.4.1"]
                 [compojure "1.4.0"]
                 [liberator "0.13"]
                 [hiccup "1.0.5"]
                 [garden "1.2.6" :exclusions [org.clojure/clojure]]
                 [clj-time "0.10.0"]
                 [environ "1.0.0"]
                 [thi.ng/trio "0.2.0-SNAPSHOT"]
                 [thi.ng/validate "0.1.3"]
                 [thi.ng/color "0.3.1"]
                 [org.clojars.toxi/markdown-clj "0.9.57" :exclusions [org.clojure/clojure]]

                 [reagent "0.5.0"]
                 [org.clojars.toxi/re-frame "0.2.0"]
                 ;;[net.coobird/thumbnailator "0.4.8"]
                 ;;[cljsjs/codemirror "5.1.0-2"]
                 ]

  :main          ea.production

  :clean-targets ^{:protect false} ["resources/public/js/compiled"]

  :profiles {:dev
             {:dependencies   [[javax.servlet/servlet-api "2.5"]
                               [ring/ring-mock "0.2.0"]]

              :plugins        [[lein-cljsbuild "1.0.6"]
                               [lein-figwheel "0.3.3"]]

              :resource-paths ["config/dev"]

              :repl-options   {:init-ns user}

              :figwheel       {:css-dirs ["resources/public/css"]
                               ;;:ring-handler metaseams.handler/app
                               }

              :cljsbuild {:builds [{:id "dev"
                                    :source-paths ["src-cljs"]
                                    :figwheel {:on-jsload "ea.core/main"}
                                    :compiler {:main ea.core
                                               :output-to "resources/public/js/compiled/app.js"
                                               :output-dir "resources/public/js/compiled/out"
                                               :asset-path "/js/compiled/out"
                                               :source-map-timestamp true}}]}}

             :uberjar
             {:hooks          [leiningen.cljsbuild]

              :uberjar-name   "ea-standalone.jar"

              :resource-paths ["config/prod"]

              ;;:aot            :all

              ;;:omit-source    true

              :cljsbuild      {:builds [{:id "prod"
                                         :source-paths ["src-cljs"]
                                         :compiler {:main ea.core
                                                    :output-to "resources/public/js/compiled/app.js"
                                                    :optimizations :advanced
                                                    :pretty-print false
                                                    :externs ["externs/syntax.js"]}}]}}})
