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
   [thi.ng.trio.vocabs.dcterms :refer [dcterms]]
   [clojure.edn :as edn]
   [clojure.data.json :as json]
   [clojure.string :as str]
   [clojure.pprint :refer [pprint]]
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
                     oname (subs % (inc s))
                     puri (vu/expand-pname prefixes pname)
                     ouri (vu/expand-pname prefixes oname)]
                 (if puri
                   [puri (or ouri oname)]
                   (warn "skipping unknown prefix in pname:" pname))))))
         (filter identity)
         (reduce
          (fn [acc [k v]] (update-in acc [k] (fnil conj []) v)) {}))))

(defn handle-resource-update
  [ctx]
  (info :post (get-in ctx [:request :params]))
  (let [{:keys [id attribs bulk-attribs replace]} (get-in ctx [:request :params])
        {:keys [prefixes graph]} @state
        now (t/now)
        fnow (view/format-date now)
        attribs (reduce-kv
                 (fn [acc k v]
                   (if-let [uri (vu/expand-pname prefixes k)]
                     (assoc acc uri v)
                     acc))
                 {} attribs)
        attribs (if (model/new-resource? id)
                  (assoc attribs (:created dcterms) fnow)
                  (assoc attribs (:modified dcterms) fnow))
        attribs (merge attribs (parse-attribs bulk-attribs))
        src-triples (set (trio/select graph id nil nil))
        new-triples (->> attribs (map (fn [[p o]] (trio/triple id (name p) o))) (trio/triple-seq))
        triples (reduce
                 (fn [acc t]
                   (let [p (nth t 1)]
                     (info :triple t :p p)
                     (if-let [del (seq (filter #(and (not= t %) (= p (:p %))) acc))]
                       (let [acc (apply disj acc del)]
                         (info :delete del)
                         (if-not (empty? (last t)) (conj acc t) acc))
                       (if-not (empty? (last t)) (conj acc t) acc))))
                 src-triples
                 new-triples)]
    (info :src)
    (pprint src-triples)
    (info :new)
    (pprint new-triples)
    (info :updated)
    (pprint triples)
    (dosync
     (alter state
            (fn [{:keys [prefixes graph] :as state}]
              (let [graph (-> graph
                              (trio/remove-triples src-triples)
                              (trio/add-triples triples))]
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
