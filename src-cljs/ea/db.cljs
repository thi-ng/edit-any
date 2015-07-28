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
   (-> db
       (assoc-in [:session :current-page] route)
       (dissoc :resource-form :resource-form-edits))))

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
   (-> db
       (assoc-in [:resource-form id] v)
       (assoc-in [:resource-form-edits id] true))))

(register-handler
 :set-resource-field
 (fn [db [_ id v]]
   (info :set id v)
   (assoc-in db [:resource-form id] v)))

(register-handler
 :submit-resource-update
 (fn [db _]
   (let [edits (:resource-form-edits db)
         form  (reduce-kv (fn [acc k v] (if (edits k) (assoc acc k v) acc)) {} (:resource-form db))]
     (info :edited-fields form)
     (when (seq form)
       (utils/do-request
        {:uri     (server-route (-> db :session :current-page))
         :method  :post
         :data    form
         :success (fn [_ data] (dispatch [:resource-updated data]))})))
   db))

(register-handler
 :resource-updated
 (fn [db [_ res]]
   (dispatch [:resource-loaded res])
   db))
