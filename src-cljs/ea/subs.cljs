(ns ea.subs
  (:require-macros
   [reagent.ratom :refer [reaction]]
   [cljs-log.core :refer [debug info warn]])
  (:require
   [clojure.string :as str]
   [reagent.core :as reagent :refer [atom]]
   [re-frame.core :refer [register-sub subscribe dispatch]]))

(defn path-subscription
  [id path]
  (info :register-sub id path)
  (register-sub id (fn [db _] (reaction (get-in @db path)))))

(path-subscription :current-page              [:session :current-page])
(path-subscription :current-resource          [:session :current-resource])
(path-subscription :current-resource-view-tab [:ui :current-resource-view-tab])
(path-subscription :current-user              [:session :user])
(path-subscription :all-predicates            [:predicates])

(register-sub
 :app-ready?
 (fn [db _] (reaction (and (:inited @db) (:router-started @db)))))
