(ns ea.db
  (:require-macros
   [reagent.ratom :refer [reaction]]
   [cljs-log.core :refer [debug info warn]])
  (:require
   [ea.router :as router]
   [ea.session :as session]
   [ea.utils :as utils]
   [ea.model :as model]
   [ea.templates :as tpl]
   [thi.ng.validate.core :as v]
   [re-frame.core :refer [register-handler subscribe dispatch]]
   [clojure.string :as str]
   [reagent.core :as reagent]))

(defn server-route
  [route]
  (str "/" (router/format-route route (:params route))))

(register-handler
 :init-app
 (fn [db [_ config]]
   (let [db (if-let [session (utils/safe-read-string (aget js/window "__EA_SESSION__"))]
              (session/update-session-user db session)
              db)])
   (dispatch [:start-router])
   (assoc db
          :config config
          :ui     {:current-resource-view-tab :content}
          :inited true)))

(register-handler
 :start-router
 (fn [db _]
   (let [config (:config db)
         routes (:routes config)]
     (router/start!
      routes
      (routes (:default-route-index config))
      (:default-route config)
      #(dispatch [:nav-change %])
      (subscribe [:current-user]))
     (assoc db :router-started true))))

(register-handler
 :nav-change
 (fn [db [_ route]]
   (info :nav-change (dissoc route :validate :component))
   (assoc-in db [:session :current-page] route)))

(register-handler
 :nav-trigger
 (fn [db [_ route]]
   (info :nav-trigger route)
   (router/trigger! route)
   db))

(register-handler
 :load-resource
 (fn [db [_ route]]
   (utils/do-request
    {:uri     (server-route route)
     :success (fn [_ data] (dispatch [:resource-loaded data]))})
   db))

(register-handler
 :resource-loaded
 (fn [db [_ res]]
   (info :success res)
   (let [{:keys [body tpl shared-pred shared-obj]} res
         res (if tpl (tpl/build-resource-template res) res)
         tab (cond
               (seq body)                              :content
               (model/is-template? res)                :tpl
               (or (seq shared-pred) (seq shared-obj)) :related
               :else :edit)]
     (-> db
         (assoc-in [:session :current-resource] res)
         (assoc-in [:ui :current-resource-view-tab] tab)))))

(register-handler
 :select-resource-view-tab
 (fn [db [_ id]]
   (info :select-tab id)
   (assoc-in db [:ui :current-resource-view-tab] id)))

(register-handler
 :resource-field-edit
 (fn [db [_ id v]]
   (info :edit id v)
   (assoc-in db [:form id] v)))
