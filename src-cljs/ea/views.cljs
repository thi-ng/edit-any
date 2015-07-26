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
    (info :reslink id :pname pname :resu res-uri :uri? uri? :label label)
    (if uri?
      (list
       [:a {:href res-uri :title res-uri} label] " "
       (if (and (or uri? pname) (not= 0 (.indexOf id (prefixes "this"))))
         [:a {:href id :title id}
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
          [:h5.attrib (resource-link prefixes attr ((first vals) '?atitle) 30)]
          ;;[:h5.attrib attr]
          [:ul
           (map
            (fn [{:syms [?val ?vtitle]}]
              (info :attr ?val ?vtitle)
              [:li
               (resource-link prefixes ?val ?vtitle 30) " "
               ;;?val " "
               [:a.delete {:href "#"} [:span.glyphicon.glyphicon-remove]]])
            vals)]))
       (sort-by key attribs))]]))

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
           [md/preview (first (:body @res))]]
          [attrib-sidebar @res]]]
        [:div "Loading resource: " (-> route :params :id)]))))
