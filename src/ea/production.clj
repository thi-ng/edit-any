(ns ea.production
  (:require
   [ea.core :as ea]
   [com.stuartsierra.component :as comp]
   [clojure.tools.namespace.repl :refer (refresh)]
   [taoensso.timbre :refer [info warn error]]))

(defn -main [& args]
  (-> (ea/get-config)
      ea/make-system
      comp/start))
