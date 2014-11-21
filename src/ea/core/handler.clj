(ns ea.core.handler
  (:require
   [ea.core.model :as model :refer [state]]
   [ea.core.templates :as tpl]
   [ea.core.view :as view]
   [ea.core.utils :as utils]
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

(taoensso.timbre/set-config! [:ns-blacklist] ['thi.ng.trio.query])

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
          (fn [acc [k v]]
            (update-in acc [k] (fnil conj []) v))
          {}))))

(defn replace-triples
  [src new]
  (reduce
   (fn [acc t]
     (let [p (nth t 1)]
       (info :triple t :p p)
       (if-let [del (->> src
                         (filter #(and (not= t %) (= p (:p %))))
                         (seq))]
         (let [acc (apply disj acc del)]
           (info :delete del)
           (if-not (empty? (last t)) (conj acc t) acc))
         (if-not (empty? (last t)) (conj acc t) acc))))
   (set src) new))

(defn handle-resource-update
  [ctx]
  (info :post (get-in ctx [:request :params]))
  (let [{:keys [id attribs bulk-attribs replace]} (get-in ctx [:request :params])
        {:keys [prefixes graph]} @state
        res (if-let [uri (vu/expand-pname prefixes id)] uri (str (prefixes "this") id))
        now (t/now)
        fnow (utils/format-date now)
        attribs (reduce-kv
                 (fn [acc k v]
                   (if-let [uri (vu/expand-pname prefixes k)]
                     (assoc acc uri (mapv utils/line-endings (if (vector? v) v [v])))
                     acc))
                 {} attribs)
        auto-triples (if (model/new-resource? res)
                       [[res (:created dcterms) fnow]]
                       [[res (:modified dcterms) fnow]])
        attribs (merge attribs (parse-attribs bulk-attribs))
        _ (info :attribs)
        _ (pprint attribs)
        src-triples (trio/select graph res nil nil)
        new-triples (->> attribs
                         (map (fn [[p o]] [res (name p) o]))
                         (trio/triple-seq))
        merged-triples (replace-triples src-triples auto-triples)
        merged-triples (if replace
                         (replace-triples merged-triples new-triples)
                         (->> (into merged-triples new-triples)
                              (filter #(not (empty? (last %))))))]
    (info :src)
    (pprint src-triples)
    (info :new)
    (pprint new-triples)
    (info :updated)
    (pprint merged-triples)
    (model/update! src-triples merged-triples)
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
        id (-> ctx :request :params :id)
        __ (info :id (vu/expand-pname prefixes id) id)
        id (if-let [uri (vu/expand-pname prefixes id)] uri (str (prefixes "this") id))
        __ (info :id id (-> ctx :request :uri))
        {:syms [?body ?title] :as res} (model/get-resource-description graph id)
        ?title (or ?title
                   (if-let [pn (vu/find-prefix prefixes id)]
                     (if (= "this" (pn 0)) (if (seq (pn 1)) (pn 1)) (model/format-pname pn))))
        template (tpl/build-resource-template prefixes graph id)
        attr-tpls (model/get-attrib-templates prefixes graph)
        attribs (model/get-other-resource-attribs graph id)
        shared-pred (model/get-shared-predicate graph id)
        shared-obj (model/get-shared-object graph id)
        res-uri (first (model/resource-uri prefixes id))]
    ;;(info id :title ?title)
    (info :res-uri res-uri)
    (info :prefixes prefixes)
    (info id :attribs)
    (pprint attribs)
    ;;(info id :shared-p shared-pred)
    ;;(info id :shared-o shared-obj)
    (view/html-template
     [:div.row
      [:div.col-xs-12 [:h1 ?title]]]
     [:form {:method :post :action res-uri}
      [:div.row
       [:div.col-sm-8.col-md-9
        (view/content-tab-panels ?body template)
        (when (or (seq shared-pred) (seq shared-obj))
          (view/related-resource-table prefixes (or ?title id) shared-pred shared-obj))]
       (view/attrib-sidebar prefixes graph attribs attr-tpls)]])))

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
