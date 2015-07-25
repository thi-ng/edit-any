(ns ea.core.server
  (:require
   [ea.core.protocols :as proto]
   [thi.ng.xerror.core :as err]
   [aleph.http :as http]
   [com.stuartsierra.component :as comp]
   [taoensso.timbre :refer [info warn error]]))

(defrecord AlephServer [config server handler]
  comp/Lifecycle
  (start [_]
    (if-not server
      (let [port   (:port config)
            routes (proto/route-map handler)]
        (info "starting aleph server on port:" port)
        (assoc _ :server (http/start-server routes {:port port})))
      (do (warn "aleph already running...")
          _)))
  (stop [_]
    (if server
      (do (info "stopping server...")
          (.close ^java.io.Closeable server)
          (assoc _ :server nil))
      (do (warn "server already stopped!")
          _))))

(defn make-server
  [config]
  (condp = (:type config)
    :aleph (AlephServer. config nil nil)
    (err/unsupported! (str "Unsupported server type: " (:type config)))))
