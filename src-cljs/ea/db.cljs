(ns ea.db
  (:require-macros
   [reagent.ratom :refer [reaction]]
   [cljs-log.core :refer [debug info warn]])
  (:require
   [ea.router :as router]
   [ea.session :as session]
   [ea.utils :as utils]
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
   ;;(dispatch [:nav-close-all])
   (-> db
       (assoc-in [:session :current-page] route)
       (assoc-in [:session :current-resource] nil))))

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
     :success (fn [_ data]
                (dispatch [:resource-loaded data]))})
   (assoc-in db [:session :current-resource] nil)))

(register-handler
 :resource-loaded
 (fn [db [_ data]]
   (info :success data)
   (assoc-in db [:session :current-resource] data)))
