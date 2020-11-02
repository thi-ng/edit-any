(ns ea.core.handler2
  (:require
   [ea.core.protocols :as proto]
   [ea.core.model :as model]
   [ea.core.views2 :as views]
   [ea.core.instrument :as instr]
   [ea.core.utils :as utils]
   [thi.ng.trio.vocabs.utils :as vu]
   [manifold.stream :as s]
   [manifold.deferred :as d]
   [manifold.bus :as bus]
   [aleph.http :as http]
   [compojure.core :as compojure :refer [GET POST]]
   [compojure.route :as route]
   [liberator.conneg :as conneg]
   [ring.util.response :as resp]
   [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
   [ring.middleware.stacktrace :refer [wrap-stacktrace]]
   [clojure.string :as str]
   [clojure.core.async :as async :refer [go go-loop <! >! close! chan timeout]]
   [clojure.pprint :refer [pprint]]
   [com.stuartsierra.component :as comp]
   [taoensso.timbre :refer [info warn error]]))

(defn negotiate-media-type
  [req]
  (as-> (get-in req [:headers "accept"]) accept
    (conneg/best-allowed-content-type accept ["text/html" "application/edn"])
    (str/join "/" accept)))

(defn get-predicates
  [req model]
  (let [preds    (model/all-predicates model)
        prefixes (proto/prefix-map model)]
    (-> {:predicates preds
         :prefixes   prefixes}
        (pr-str)
        (resp/response)
        (resp/content-type "application/edn"))))

(defn get-resource-edn
  [req model]
  (let [id          (-> req :params :id)
        uri         (model/as-resource-uri model id)
        res         (model/resource-title-body model uri)
        title       (model/resource-title model res uri)
        tpl-spec    (model/resource-template model uri)
        tpl-results (when tpl-spec
                      (model/execute-resource-tpl-queries model uri tpl-spec))
        attr-tpls   (model/all-attrib-templates model)
        attribs     (model/other-resource-attribs model uri)
        shared-pred (model/facet-shared-predicate model uri)
        shared-obj  (model/facet-shared-object model uri)
        res-uri     (first (model/canonical-resource-uri model uri))]
    (-> {:id          id
         :uri         uri
         :body        (res '?body)
         :res-uri     res-uri
         :title       title
         :tpl         tpl-spec
         :tpl-results tpl-results
         :attribs     attribs
         :attr-tpl    attr-tpls
         :shared-pred shared-pred
         :shared-obj  shared-obj
         :prefixes    (proto/prefix-map model)}
        (pr-str)
        (resp/response)
        (resp/content-type "application/edn"))))

(defn get-resource
  [req model config]
  (let [accept (negotiate-media-type req)]
    (cond
      (= "application/edn" accept) (get-resource-edn req model)
      :else                        (views/html-template req config))))

(defn update-resource
  [req model config]
  (let [id       (-> req :params :id)
        params   (:form-params req)
        [old new :as diff] (model/compute-resource-changeset model id params)]
    (info :update id params)
    (info :diff)
    (pprint diff)
    ;;(proto/update-graph model old new)
    (get-resource-edn req model)))

(defn build-routes
  [config model]
  (compojure/routes
   (GET "/" [] (resp/redirect (str "/resources/Index")))
   (GET "/predicates" [req]
        (get-predicates req model))
   (GET ["/resources/:id" :id #".*"] [id :as req]
        (let [[resp time] (instr/timed-action (get-resource req model config))]
          (info :response-time time id)
          resp))
   (POST ["/resources/:id" :id #".*"] [id :as req]
         (let [[resp time] (instr/timed-action (update-resource req model config))]
           (info :response-time time id)
           resp))
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
          routes   (if (:dev config)
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
