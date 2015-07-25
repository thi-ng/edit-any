(ns ea.core.app
  (:require
   [com.stuartsierra.component :as comp]
   [taoensso.timbre :refer [info warn error]]))

(defrecord App [config]
  comp/Lifecycle
  (start [_]
    (info "starting app...")
    _)
  (stop [_]
    (info "stopping app...")
    _))

(defn make-app
  [config] (App. config))
