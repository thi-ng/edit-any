(ns ea.core
  (:require
   [ea.core.app :as app]
   [ea.core.model :as model]
   [ea.core.handler2 :as handler]
   [ea.core.server :as server]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [environ.core :as environ]
   [com.stuartsierra.component :as comp]
   [taoensso.timbre :refer [info warn error]])
  (:import
   java.io.PushbackReader))

(def system nil)

(def default-config
  {:db      {:type :memory :path "graph.edn"}
   :server  {:type :aleph :port 3000}
   :handler {:dev true
             :hljs-style-uri "/js/highlight/styles/solarized_light.css"
             :google-analytics-id "UA-41184058-1"}})

(defn- read-config-file []
  (try
    (with-open [r (-> "config.edn" io/resource io/reader PushbackReader.)]
      (edn/read r))
    (catch Exception e
      (println (str "WARNING: edn-config: " (.getLocalizedMessage e))))))

(defn get-config
  []
  (taoensso.timbre/merge-config! {:ns-blacklist ["thi.ng.trio.query"]})
  (merge default-config (read-config-file) environ/env))

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
