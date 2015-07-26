(ns user
  (:require
   [ea.core :as ea]
   [com.stuartsierra.component :as comp]
   [clojure.tools.namespace.repl :refer (refresh)]
   [taoensso.timbre :refer [info warn error]]))

(defn init []
  (alter-var-root
   #'ea/system
   (constantly (ea/make-system (ea/get-config)))))

(defn start []
  (alter-var-root #'ea/system comp/start))

(defn stop []
  (alter-var-root
   #'ea/system
   (fn [s] (when s (comp/stop s)))))

(defn go []
  (init)
  (start))

(defn reset []
  (stop)
  (refresh :after 'user/go))
