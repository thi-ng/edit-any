(ns ea.core
  (:require
   [ea.core.app :as app]
   [ea.core.model :as model]
   [ea.core.handler2 :as handler]
   [ea.core.server :as server]
   [com.stuartsierra.component :as comp]
   [taoensso.timbre :refer [info warn error]]))

(def system nil)

(defn get-config
  []
  (taoensso.timbre/merge-config! {:ns-blacklist ["thi.ng.trio.query"]})
  {:db      {:type :memory :path "graph.edn"}
   :server  {:type :aleph :port 3000}
   :handler {:dev true
             :google-analytics-id "UA-41184058-1"}})

(defn make-system
  [config]
  (comp/system-map
   :db      (model/make-db (:db config))
   :handler (comp/using
             (handler/make-handler (:handler config))
             {:model :db})
   :server  (comp/using
             (server/make-server (:server config))
             {:handler :handler})
   :app     (app/make-app config)))
