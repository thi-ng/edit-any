(ns ea.core.views2
  (:require
   [clojure.string :as str]
   [hiccup.page :refer [html5 include-js include-css]]
   [hiccup.element :as el]))

(def CACHE-BUSTER (str \? (System/currentTimeMillis))) ;; TODO use git rev

(def fontawesome-uri
  "//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css")

(defn with-cache-buster
  [f & uris]
  (apply f (eduction (map #(str % CACHE-BUSTER)) uris)))

(defn ga-tracker
  [id]
  (when id
    (el/javascript-tag
     (str "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/analytics.js','ga');ga('create', '" id "', 'auto');ga('send', 'pageview');"))))

(defn html-template
  [req config]
  (html5
   [:head
    [:meta {:charset "utf-8"}]
    [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
    [:meta {:name "viewport" :content "width=device-width,initial-scale=1.0,maximum-scale=1.0,user-scalable=0"}]
    [:meta {:name "author" :content "Karsten Schmidt, thi.ng"}]
    (include-css fontawesome-uri)
    (with-cache-buster include-css
      "/css/bootstrap.min.css"
      "/css/main.css"
      "/highlight/styles/solarized_light.css")
    (apply include-js ["/highlight/highlight.pack.js"])]
   [:body
    [:div#app.container-fluid
     [:div.row
      [:div.col-xs-12
       [:i.fa.fa-spinner.fa-spin] " Loading..."]]]
    (el/javascript-tag "// var __XSRF_TOKEN__;")
    (with-cache-buster include-js "/js/app.js")
    (ga-tracker (:google-analytics-id config))]))
