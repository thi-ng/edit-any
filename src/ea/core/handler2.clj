(ns ea.core.handler2
  (:require
   [ea.core.protocols :as proto]
   [ea.core.model :as model]
   [ea.core.views2 :as views]
   [thi.ng.trio.vocabs.utils :as vu]
   [manifold.stream :as s]
   [manifold.deferred :as d]
   [manifold.bus :as bus]
   [aleph.http :as http]
   [compojure.core :as compojure :refer [GET ANY]]
   [compojure.route :as route]
   [liberator.conneg :as conneg]
   [ring.util.response :as resp]
   [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
   [ring.middleware.stacktrace :refer [wrap-stacktrace]]
   [clojure.string :as str]
   [clojure.core.async :as async :refer [go go-loop <! >! close! chan timeout]]
   [com.stuartsierra.component :as comp]
   [taoensso.timbre :refer [info warn error]]))

(def pprint-str #(with-out-str (clojure.pprint/pprint %)))

(defn negotiate-media-type
  [req]
  (as-> (get-in req [:headers "accept"]) accept
    (conneg/best-allowed-content-type accept ["text/html" "application/edn"])
    (str/join "/" accept)))

(defn handle-resource-get
  [req model accept]
  (let [id          (-> req :params :id)
        uri         (model/as-resource-uri model id)
        res         (model/resource-title-body model uri)
        title       (model/resource-title model res uri)
        ;;tpl         (tpl/build-resource-template prefixes graph id)
        ;;attr-tpls   (model/get-attrib-templates prefixes graph)
        attribs     (model/other-resource-attribs model uri)
        shared-pred (model/facet-shared-predicate model uri)
        shared-obj  (model/facet-shared-object model uri)
        res-uri     (first (model/canonical-resource-uri model uri))]
    (-> {:id id
         :uri uri
         :body (res '?body)
         :res-uri res-uri
         :title title
         :attribs  attribs
         ;;:attr-tpl attr-tpls
         :preds shared-pred
         :obj   shared-obj}
        (pr-str)
        (resp/response)
        (resp/content-type accept))))

(defn build-routes
  [config model]
  (compojure/routes
   (GET "/" [] (resp/redirect (str "/resources/Index")))
   (GET ["/resources/:id" :id #".*"] [:as req]
        (let [accept (negotiate-media-type req)]
          (cond
            (= "application/edn" accept) (handle-resource-get req model accept)
            :else                        (views/html-template req config))))
   (route/resources "/")
   (route/not-found "404")))

(defrecord Handler [config model routes]
  comp/Lifecycle
  (start [_]
    (info "starting handler...")
    (let [routes   (build-routes config model)
          defaults (assoc-in site-defaults [:security :anti-forgery] false)
          routes   (-> routes
                       (wrap-defaults defaults))
          routes (if (:dev config)
                   (wrap-stacktrace routes)
                   routes)]
      (assoc _ :routes routes)))
  (stop [_]
    (info "stopping handler..."))
  proto/IHandler
  (route-map [_]
    routes))

(defn make-handler
  [config] (Handler. config nil nil))