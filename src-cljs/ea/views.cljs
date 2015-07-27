(ns ea.views
  (:require-macros
   [reagent.ratom :refer [reaction]]
   [cljs-log.core :refer [debug info warn]])
  (:require
   [ea.router :as router]
   [ea.markdown :as md]
   [ea.model :as model]
   [ea.utils :as utils]
   [re-frame.core :refer [subscribe dispatch]]
   [clojure.string :as str]))

(defn resource-link
  [prefixes id label & [trunc]]
  (let [[res-uri pname uri?] (model/resource-uri prefixes id)
        label (if label
                (utils/truncate trunc label)
                (if uri?
                  (or pname (subs id (count (ffirst uri?))))
                  (utils/truncate trunc id)))]
    ;;(info :reslink id :pname pname :resu res-uri :uri? uri? :label label)
    (if uri?
      (list
       [:a {:key res-uri :href res-uri :title res-uri
            :on-click (utils/prevent
                       #(dispatch [:nav-trigger (str "resources/" (or pname id))]))}
        label] " "
        (if (and (or uri? pname) (not= 0 (.indexOf id (prefixes "this"))))
          [:a {:key id :href id :title id}
           [:span.glyphicon.glyphicon-new-window]]))
      label)))

(defn attrib-sidebar
  [res]
  (let [{:keys [prefixes attribs]} res]
    [:div#sidebar.col-sm-4.col-md-3
     [:h4 "Attributes "
      [:span.label.label-default (reduce #(+ % (count (val %2))) 0 attribs)]]
     [:div.attribs
      (map
       (fn [[attr vals]]
         (list
          [:h5.attrib {:key (str "hd-" attr)} (resource-link prefixes attr ((first vals) '?atitle) 30)]
          [:ul {:key (str "ul-" attr)}
           (map
            (fn [{:syms [?val ?vtitle]}]
              [:li {:key (str attr ?val)}
               (resource-link prefixes ?val ?vtitle 30) " "
               [:a.delete {:href "#"} [:span.glyphicon.glyphicon-remove]]])
            vals)]))
       (sort-by key attribs))]]))

(defn related-resource-table
  [res]
  (let [{:keys [prefixes id title shared-pred shared-obj]} res]
    [:div
     [:h3 "Related resources "
      [:span.label.label-default (+ (count shared-pred) (count shared-obj))]]
     [:table.table.table-striped
      [:tbody
       (map
        (fn [{:syms [?other ?otitle ?val ?vtitle]}]
          [:tr {:key (str ?other ?val)}
           [:td (resource-link prefixes ?other ?otitle)]
           [:td (resource-link prefixes id title)]
           [:td (resource-link prefixes ?val ?vtitle)]])
        shared-pred)
       (map
        (fn [{:syms [?other ?otitle ?pred ?ptitle]}]
          [:tr {:key (str ?other ?pred)}
           [:td (resource-link prefixes ?other ?otitle)]
           [:td (resource-link prefixes ?pred ?ptitle)]
           [:td (resource-link prefixes id title)]])
        shared-obj)]]]))

(defn resource-editor
  [body]
  [:div#resource-editor
   [:h3 "Edit resource description"]
   [:p
    [:textarea.form-control
     {:name "attribs[dcterms:description]" :rows 10
      :defaultValue (first body)}]]
   [:p
    [:button.btn.btn-primary {:type "submit"} "Submit"]]])

(defn tab-header
  [sel id title]
  [:li
   {:role "presentation" :class (when (= id @sel) "active")}
   [:a
    {:href "#"
     :role "tab"
     :on-click (utils/prevent #(dispatch [:select-resource-view-tab id]))}
    title]])

(defn tab-pane
  [sel id body]
  (when (= id @sel)
    [:div.tab-pane {:role "tabpanel" :class (when (= id @sel) "active")} body]))

(defn content-tab-panels
  [res]
  (let [sel (subscribe [:current-resource-view-tab])]
    (fn [res]
      (let [{:keys [prefixes body tpl shared-pred shared-obj]} res
            related? (or (seq shared-pred) (seq shared-obj))]
        (info :tab-sel @sel)
        [:div {:role "tabpanel"}
         [:ul.nav.nav-tabs {:role "tablist"}
          [tab-header sel :content "Content"]
          [tab-header sel :edit "Edit"]
          [tab-header sel :viz "Graph"]
          (when tpl [tab-header sel :tpl "Template"])
          (when related? [tab-header sel :related "Related"])]
         [:div.tab-content
          [tab-pane sel :content [md/preview (first body)]]
          [tab-pane sel :edit [resource-editor body]]
          [tab-pane sel :viz
           [:div#resource-viz
            [:h3 "Resource graph"]
            [:div.well [:h3 "TODO"]]]]
          (when tpl
            [tab-pane sel :tpl [:div#resource-tpl (first tpl)]])
          (when related?
            [tab-pane sel :related (related-resource-table res)])]]))))

(defn resource-view
  [route]
  (let [res (subscribe [:current-resource])]
    (dispatch [:load-resource route])
    (fn [route]
      (if @res
        [:div
         [:div.row
          [:div.col-xs-12 [:h1 (:title @res)]]]
         [:div.row
          [:div.col-sm-8.col-md-9
           [content-tab-panels @res]]
          [attrib-sidebar @res]]]
        [:div "Loading resource: " (-> route :params :id)]))))
