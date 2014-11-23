(ns ea.core.view
  (:require
   [ea.core.model :as model]
   [ea.core.utils :as utils]
   [thi.ng.trio.core :as trio]
   [thi.ng.trio.vocabs.utils :as vu]
   [clojure.string :as str]
   [hiccup.page :refer [html5 include-js include-css]]
   [hiccup.element :as el]
   [hiccup.form :as form]
   [markdown.core :as md]
   [markdown.transformers :as mdtx]))

;; pname? (re-seq #"^([a-zA-Z0-9]+:[a-zA-Z0-9]+)" id)

(defn resource-link
  [prefixes id label & [trunc]]
  (let [[res-uri pname uri?] (model/resource-uri prefixes id)
        label (if label
                (utils/truncate trunc label)
                (if uri?
                  (or pname (subs id (count (ffirst uri?))))
                  (utils/truncate trunc id)))]
    (prn :reslink id :pname pname :resu res-uri :uri? uri? :label label)
    (if uri?
      (list
       [:a {:href res-uri :title res-uri} label] " "
       (if (and (or uri? pname) (not (.startsWith id (prefixes "this"))))
         [:a {:href id :title id}
          [:span.glyphicon.glyphicon-new-window]]))
      label)))

(defn html-template
  [id & body]
  (html5
   [:head
    (apply include-css ["/css/bootstrap.min.css" "/css/main.css" "/highlight/styles/solarized_light.css"])
    (apply include-js ["/js/jquery-2.1.1.min.js" "/js/bootstrap.min.js" "/js/marked.min.js" "/highlight/highlight.pack.js"])]
   [:body {:resource id}
    [:div.container-fluid body]
    (el/javascript-tag
     "hljs.initHighlightingOnLoad();
$(\"#editor\").blur(function(e){$(\"#content-body\").html(marked(e.target.value));});
$(\"#attr-templates\").change(function(e){if (e.target.value!=\"\") $(\"#new-attribs\").val(e.target.value);});")
    ;;(include-js "/js/app.js")
    ]))

(defn attrib-sidebar
  [prefixes graph attribs templates]
  [:div#sidebar.col-sm-4.col-md-3
   [:h4 "Attributes "
    [:span.label.label-default (reduce #(+ % (count (val %2))) 0 attribs)]]
   [:div.attribs
    (map
     (fn [[attr vals]]
       (prn :attr (first vals))
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
   [:h4 "Add attributes"]
   [:div.form-group [:textarea#new-attribs.form-control {:name "bulk-attribs" :rows 5}]]
   [:select#attr-templates.form-control (map (fn [{:keys [id tpl]}] [:option {:label id :value tpl}]) (cons {:id "Choose template..":value ""} templates))]
   [:div.checkbox
    [:label [:input {:type "checkbox" :name "replace" :checked "checked"}] " Replace existing"]]
   [:div.form-group [:button.btn.btn-primary {:type "submit"} "Submit"]]])

(defn content-tab-panels
  [prefixes body tpl]
  [:div {:role "tabpanel"}
   [:ul.nav.nav-tabs {:role "tablist"}
    [:li.active {:role "presentation"} [:a {:href "#content" :role "tab" :data-toggle "tab"} "Content"]]
    ;;[:li {:role "presentation"} [:a {:href "#edit" :role "tab" :data-toggle "tab"} "Edit"]]
    [:li {:role "presentation"} [:a {:href "#viz" :role "tab" :data-toggle "tab"} "Graph"]]
    (if tpl [:li {:role "presentation"} [:a {:href "#tpl" :role "tab" :data-toggle "tab"} "Template"]])]
   [:div.tab-content
    [:div#content.tab-pane.fade.in.active {:role "tabpanel"}
     [:div#content-body
      (md/md-to-html-string
       body
       :replacement-transformers
       (cons (utils/md-link prefixes) mdtx/transformer-vector))]]
    #_[:div#edit.tab-pane.fade {:role "tabpanel"}
       [:h3 "Edit resource description"]
       [:p [:textarea#editor.form-control {:name "attribs[dcterms:description]" :rows 10} body]]
       [:p [:button.btn.btn-primary {:type "submit"} "Submit"]]]
    [:div#viz.tab-pane.fade {:role "tabpanel"}
     [:h3 "Resource graph"]
     [:div.well [:h2 "TODO"]]]
    (if tpl [:div#tpl.tab-pane.fade {:role "tabpanel"} tpl])]])

(defn related-resource-table
  [prefixes id title shared-pred shared-obj]
  (prn :table-id id)
  (list
   [:h3 "Related resources "
    [:span.label.label-default (+ (count shared-pred) (count shared-obj))]]
   [:table.table.table-striped
    (map
     (fn [{:syms [?other ?otitle ?val ?vtitle]}]
       [:tr
        [:td (resource-link prefixes ?other ?otitle)]
        [:td (resource-link prefixes id title)]
        [:td (resource-link prefixes ?val ?vtitle)]])
     shared-pred)
    (map
     (fn [{:syms [?other ?otitle ?pred ?ptitle]}]
       [:tr
        [:td (resource-link prefixes ?other ?otitle)]
        [:td (resource-link prefixes ?pred ?ptitle)]
        [:td (resource-link prefixes id title)]])
     shared-obj)]))
