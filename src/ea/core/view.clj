(ns ea.core.view
  (:require
   [thi.ng.trio.core :as trio]
   [clojure.string :as str]
   [hiccup.page :refer [html5 include-js include-css]]
   [hiccup.element :as el]
   [hiccup.form :as form]
   [markdown.core :as md]))

(defn resource-link
  [id & [label]]
  (let [id (if (keyword? id) (name id) (str id))
        uri? (re-find #"^((https?|mailto|ftp)://|([a-zA-Z0-9]+:[a-zA-Z0-9]+))" id)
        uri (str "/resources/" id)]
    (list
     [:a {:href uri} (or label id)] " "
     (if uri? [:a {:href id} [:span.glyphicon.glyphicon-new-window]]))))

(defn html-template
  [& body]
  (html5
   [:head
    (apply include-css ["/css/bootstrap.min.css" "/css/main.css"])
    (apply include-js ["/js/jquery-2.1.1.min.js" "/js/bootstrap.min.js" "/js/marked.min.js"])]
   [:body
    [:div.container-fluid body]
    (el/javascript-tag
     "$(\"#editor\").blur(function(e){$(\"#preview-body\").html(marked(e.target.value));})")]))

(defn attrib-sidebar
  [graph attribs]
  [:div#sidebar.col-sm-3.col-lg-2
   [:h4 "Attributes "
    [:span.label.label-default (reduce #(+ % (count (val %2))) 0 attribs)]]
   (map
    (fn [[attr vals]]
      (list
       [:h5.attrib (resource-link attr ((first vals) '?atitle))]
       (el/unordered-list
        (map
         (fn [{:syms [?val ?vtitle]}] (resource-link ?val ?vtitle))
         vals))))
    (sort-by key attribs))
   [:h4 "All attributes"]
   [:select.form-control (form/select-options (sort (trio/predicates graph)))]
   [:h4 "Add attributes"]
   [:p [:textarea.form-control {:name "new-attribs"}]]
   [:p [:button.btn.btn-primary {:type "submit"} "Submit"]]])

(defn content-tab-panels
  [body]
  [:div {:role "tabpanel"}
   [:ul.nav.nav-tabs {:role "tablist"}
    [:li.active {:role "presentation"} [:a {:href "#preview" :role "tab" :data-toggle "tab"} "Preview"]]
    [:li {:role "presentation"} [:a {:href "#edit" :role "tab" :data-toggle "tab"} "Edit"]]
    [:li {:role "presentation"} [:a {:href "#viz" :role "tab" :data-toggle "tab"} "Graph"]]]
   [:div.tab-content
    [:div#preview.tab-pane.fade.in.active {:role "tabpanel"}
     [:div#preview-body (md/md-to-html-string body)]]
    [:div#edit.tab-pane.fade {:role "tabpanel"}
     [:h3 "Edit resource description"]
     [:p [:textarea#editor.form-control {:name "body" :rows 10} body]]
     [:p [:button.btn.btn-primary {:type "submit"} "Submit"]]]
    [:div#viz.tab-pane.fade {:role "tabpanel"}
     [:h3 "Resource graph"]
     [:div.well [:h2 "TODO"]]]]])

(defn related-resource-table
  [id shared-pred shared-obj]
  (list
   [:h3 "Related resources "
    [:span.label.label-default (+ (count shared-pred) (count shared-obj))]]
   [:table.table.table-striped
    (map
     (fn [{:syms [?other ?otitle ?val ?vtitle]}]
       [:tr
        [:td (resource-link ?other ?otitle)]
        [:td id]
        [:td (resource-link ?val ?vtitle)]])
     shared-pred)
    (map
     (fn [{:syms [?other ?otitle ?pred ?ptitle]}]
       [:tr
        [:td (resource-link ?other ?otitle)]
        [:td (resource-link ?pred ?ptitle)]
        [:td id]])
     shared-obj)]))
