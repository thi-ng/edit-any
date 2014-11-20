(ns ea.core.handler
  (:require
   [ea.core.model :as model :refer [state]]
   [ea.core.templates :as tpl]
   [ea.core.view :as view]
   [compojure.core :refer :all]
   [compojure.route :as route]
   [liberator.core :as lib :refer [defresource]]
   [ring.util.response :as resp]
   [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
   [thi.ng.trio.core :as trio]
   [thi.ng.trio.query :as q]
   [thi.ng.trio.vocabs.utils :as vu]
   [clojure.edn :as edn]
   [clojure.data.json :as json]
   [clojure.string :as str]
   [clj-time.core :as t]
   [taoensso.timbre :refer [info warn error]]))

;; (taoensso.timbre/set-config! [:ns-blacklist] ['thi.ng.trio.query])

;; (taoensso.timbre/set-config! [:ns-blacklist] [])

(model/init!)

(defn date-triples
  [id dt]
  (let [[y m d] (->> [t/year t/month t/day t/hour t/minute t/second]
                     (map #(% dt))
                     (reductions #(str % "-" %2)))]
    [[id "time:year" y]
     [id "time:year-month" m]
     [id "time:year-month-day" d]]))

(defn parse-attribs
  [src]
  (let [prefixes (:prefixes @state)]
    (->> src
         (str/split-lines)
         (map
          #(let [s (.indexOf % "=")]
             (if (>= s 0)
               (let [pname (subs % 0 s)
                     uri (vu/expand-pname prefixes pname)]
                 (if uri
                   [uri (subs % (inc s))]
                   (warn "unknown prefix in pname:" pname))))))
         (filter identity)
         (reduce
          (fn [acc [k v]] (update-in acc [k] (fnil conj []) v)) {}))))

(defn handle-resource-update
  [ctx]
  (let [{:keys [id body title attribs bulk-attribs replace]} (get-in ctx [:request :params])
        {{:strs [dcterms rdfs]} :prefixes graph :graph} @state
        now (t/now)
        fnow (view/format-date now)
        triples (cond-> [[id (str dcterms "modified") fnow]]
                        (model/new-resource? id) (conj [id (str dcterms "created") fnow])
                        body                     (conj [id (str dcterms "description") body])
                        title                    (conj [id (str rdfs "label") title]))
        remove-props (cond-> #{(str dcterms "modified")}
                             body  (conj (str dcterms "description"))
                             title (conj (str rdfs "label")))
        attribs (merge (parse-attribs bulk-attribs) attribs)
        _ (info :post-attribs attribs)
        triples (->> attribs
                     (map (fn [[p o]] [id (name p) o]))
                     (concat triples)
                     (trio/triple-seq))]
    (prn :triples triples)
    (dosync
     (alter state
            (fn [{:keys [prefixes graph] :as state}]
              (let [old (q/query
                         {:construct '[[?s ?p ?o]]
                          :from graph
                          :query [{:where [['?s '?p '?o]]}]
                          :values {'?s #{id} '?p remove-props}})
                    graph (-> graph
                              (trio/remove-triples old)
                              (trio/add-triples triples))]
                ;;(prn :old old)
                ;;(prn :new triples)
                (assoc state
                  :graph graph
                  :prefixes (model/build-prefixes prefixes graph))))))
    (spit "graph.edn" (sort (trio/select (:graph @state))))
    {::id id}))


(defmulti handle-resource-get #(-> % :representation :media-type))

(defmethod handle-resource-get "application/edn"
  [ctx]
  (let [id (-> ctx :request :params :id)]
    {:body (vec (model/describe-resource id))}))

(defmethod handle-resource-get "application/json"
  [ctx]
  (let [id (-> ctx :request :params :id)]
    (json/write-str
     {:body (vec (model/describe-resource id))}
     :escape-slash false)))

(defmethod handle-resource-get "text/html"
  [ctx]
  (let [{:keys [prefixes graph]} @state
        {:strs [dcterms rdfs _]} prefixes
        id (-> ctx :request :params :id)
        __ (info :id1 (vu/expand-pname prefixes id))
        id (if-let [uri (vu/expand-pname prefixes id)] uri (str _ id))
        __ (info :id id (-> ctx :request :uri))
        {:syms [?body ?title] :as res} (model/get-resource-description graph id)
        ?title (or ?title
                   (if-let [pn (vu/find-prefix prefixes id)]
                     (if (= "_" (pn 0)) (if (seq (pn 1)) (pn 1)) (view/fmt-pname pn))))
        template (tpl/build-resource-template prefixes graph id)
        attribs (model/get-other-resource-attribs graph id)
        shared-pred (model/get-shared-predicate graph id)
        shared-obj (model/get-shared-object graph id)]
    ;;(info id :title ?title)
    ;;(info id :attribs attribs)
    ;;(info id :shared-p shared-pred)
    ;;(info id :shared-o shared-obj)
    (view/html-template
     [:div.row
      [:div.col-xs-12 [:h1 ?title]]]
     [:form {:method :post :action (str "/resources/" id)}
      [:div.row
       [:div.col-sm-8.col-md-9
        (view/content-tab-panels ?body template)
        (when (or (seq shared-pred) (seq shared-obj))
          (view/related-resource-table prefixes (or ?title id) shared-pred shared-obj))]
       (view/attrib-sidebar prefixes graph attribs (model/get-attrib-templates prefixes graph))]])))

(defresource resource [id]
  :available-media-types ["text/html" "application/edn" "application/json"]
  :allowed-methods [:get :post]
  :handle-ok handle-resource-get
  :post! handle-resource-update
  :post-redirect? (fn [ctx] {:location (str "/resources/" (::id ctx))}))

(defroutes app-routes
  (GET "/" [] (resp/redirect (str "/resources/Index")))
  (ANY ["/resources/:id" :id #".*"] [id] (resource id)))

(def app
  (->> (assoc-in site-defaults [:security :anti-forgery] false)
       (wrap-defaults app-routes)))
