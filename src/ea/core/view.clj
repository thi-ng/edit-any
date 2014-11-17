(ns ea.core.view
  (:require
   [thi.ng.trio.core :as trio]
   [thi.ng.trio.vocabs.utils :as vu]
   [clojure.string :as str]
   [hiccup.page :refer [html5 include-js include-css]]
   [hiccup.element :as el]
   [hiccup.form :as form]
   [markdown.core :as md]))

(defn resource-link
  [prefixes id label & [trunc]]
  (let [id (if (keyword? id) (name id) (str id))
        uri? (re-seq #"^(https?|mailto|ftp)://" id)
        pname? (re-seq #"^([a-zA-Z0-9]+:[a-zA-Z0-9]+)" id)
        res-uri (str "/resources/" id)
        exp-uri (if pname? (vu/expand-prefixes prefixes id) id)
        label (or label (if uri? (subs id (count (ffirst uri?))) id))
        label (if (and trunc (> (count label) trunc))
                (str (subs label 0 trunc) "\u2026")
                label)]
    (list
     [:a {:href res-uri :title res-uri} label] " "
     (if (or uri? pname?)
       [:a {:href exp-uri :title exp-uri}
        [:span.glyphicon.glyphicon-new-window]]))))

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
  [prefixes graph attribs]
  [:div#sidebar.col-sm-4.col-md-3
   [:h4 "Attributes "
    [:span.label.label-default (reduce #(+ % (count (val %2))) 0 attribs)]]
   [:div.attribs
    (map
     (fn [[attr vals]]
       (list
        [:h5.attrib (resource-link prefixes attr ((first vals) '?atitle) 30)]
        (el/unordered-list
         (map
          (fn [{:syms [?val ?vtitle]}]
            (list
             (resource-link prefixes ?val ?vtitle 30) " "
             [:a.delete {:href "#"} [:span.glyphicon.glyphicon-remove]]))
          vals))))
     (sort-by key attribs))]
   [:h4 "All used attributes"]
   [:select.form-control (form/select-options (sort (trio/predicates graph)))]
   [:h4 "Add attributes"]
   [:div.form-group [:textarea.form-control {:name "new-attribs"}]]
   [:div.checkbox [:label [:input {:type "checkbox" :name "replace"}] " Replace existing"]]
   [:div.form-group [:button.btn.btn-primary {:type "submit"} "Submit"]]])

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
  [prefixes id shared-pred shared-obj]
  (list
   [:h3 "Related resources "
    [:span.label.label-default (+ (count shared-pred) (count shared-obj))]]
   [:table.table.table-striped
    (map
     (fn [{:syms [?other ?otitle ?val ?vtitle]}]
       [:tr
        [:td (resource-link prefixes ?other ?otitle)]
        [:td id]
        [:td (resource-link prefixes ?val ?vtitle)]])
     shared-pred)
    (map
     (fn [{:syms [?other ?otitle ?pred ?ptitle]}]
       [:tr
        [:td (resource-link prefixes ?other ?otitle)]
        [:td (resource-link prefixes ?pred ?ptitle)]
        [:td id]])
     shared-obj)]))
