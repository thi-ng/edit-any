(ns ea.core
  (:require-macros
   [reagent.ratom :refer [reaction]]
   [cljs-log.core :refer [debug info warn]])
  (:require
   [ea.subs :as subs]
   [ea.db :as db]
   [ea.views :as views]
   [ea.utils :as utils]
   [thi.ng.validate.core :as v]
   [re-frame.core :refer [subscribe dispatch]]
   [clojure.string :as str]
   [reagent.core :as reagent]))

(def validators
  {:slug     [(v/matches #"^[\w\-]+$")]
   :pname    [(v/matches #"^([A-Za-z0-9\\-_]+:[A-Za-z0-9\\-_]*)|(\w+)$")]
   :uuid4    [(v/uuid4)]})

(def config
  {:routes
   [{:id        :resource-view
     :match     ["resources" :id]
     :validate  {:id {:validate (:pname validators)}}
     :auth      false
     :component views/resource-view}]
   :default-route-index 0
   :default-route "resources/Index"})

(defn current-page
  [page]
  (let [com (if-let [p @page] (:component p))
        com (if (fn? com) com (utils/aget-in js/window com))]
    ^{:key (str (:id @page) (-> @page :params :id))} [com @page]))

(defn main-panel
  []
  (let [init? (subscribe [:app-ready?])
        page  (subscribe [:current-page])]
    #(if @page
       [current-page page]
       [:div "Loading..."])))

(defn ^:export main
  []
  (dispatch [:init-app config])
  (reagent/render
   [main-panel] (.getElementById js/document "app")))
