(ns ea.core.handler
  (:require
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
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :refer [postwalk]]
   [clj-time.core :as t]
   [clj-time.format :as tf]
   [taoensso.timbre :refer [info warn error]]))

(taoensso.timbre/set-config!
 [:ns-blacklist] [])

(def state
  (ref {}))

(defn build-prefixes
  [{:strs [ea owl rdf]} graph]
  (->> (q/query
        {:select :*
         :from graph
         :query [{:where [['?vocab (str rdf "type") (str rdf "Description")]
                          ['?vocab (str rdf "about") '?uri]
                          ['?vocab (str ea "prefix") '?prefix]
                          ['?uri (str rdf "type") (str owl "Ontology")]]}]})
       (reduce (fn [acc {:syms [?prefix ?uri]}] (assoc acc ?prefix ?uri)) {})))

(defn start!
  []
  (try
    (let [graph    (->> "graph.edn" slurp edn/read-string trio/as-model)
          prefixes (build-prefixes
                    {"ea"  "http://thi.ng/owl/edit-any/"
                     "owl" "http://www.w3.org/2002/07/owl#"
                     "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"}
                    graph)]
      (dosync (alter state (constantly {:prefixes prefixes :graph graph}))))
    (catch Exception e
      (let [{:keys [prefixes triples]}
            (->> "default-graph.edn"
                 io/resource
                 vu/load-vocab-triples)]
        (dosync (alter state (constantly {:prefixes prefixes :graph (trio/as-model triples)})))))))

(start!)

(defn format-date
  [dt] (tf/unparse (tf/formatters :mysql) dt))

(defn date-triples
  [id dt]
  (let [[y m d] (->> [t/year t/month t/day t/hour t/minute t/second]
                     (map #(% dt))
                     (reductions #(str % "-" %2)))]
    [[id "time:year" y]
     [id "time:year-month" m]
     [id "time:year-month-day" d]]))

(defn maybe-number
  [x] (try (Long/parseLong x) (catch Exception e x)))

(defn valid-pname-prefix?
  [prefixes pname]
  (let [[pre] (str/split pname #":")]
    (prefixes pre)))

(defn parse-attribs
  [src]
  (let [prefixes (:prefixes @state)]
    (->> src
         (str/split-lines)
         (map
          #(let [s (.indexOf % "=")]
             (if (>= s 0)
               (let [pname (subs % 0 s)
                     [prefix] (valid-pname-prefix? prefixes pname)]
                 (if prefix
                   [pname (subs % (inc s))]
                   (warn "unknown prefix in pname:" pname))))))
         (filter identity)
         (reduce
          (fn [acc [k v]] (update-in acc [k] (fnil conj []) v)) {}))))

(defn attrib-templates
  [{:strs [ea dcterms rdf]} graph]
  (q/query
   {:select [{:id '?id} {:tpl '?tpl}]
    :from graph
    :query [{:where '[[?id (str rdf "type") (str ea "AttributeCollection")]
                      [?id (str dcterms "description") ?tpl]]}]}))
(defn new-resource?
  [id] (nil? (seq (trio/select (:graph @state) (maybe-number id) nil nil))))

(defn handle-resource-update
  [ctx]
  (let [{:keys [id body title attribs new-attribs replace]} (get-in ctx [:request :params])
        {{:strs [dcterms rdfs]} :prefixes graph :graph} @state
        now (t/now)
        fnow (format-date now)
        triples (cond-> [[id (str dcterms "modified") fnow]]
                        (new-resource? id) (conj [id (str dcterms "created") fnow])
                        body               (conj [id (str dcterms "description") body])
                        title              (conj [id (str rdfs "label") title]))
        remove-props (cond-> #{(str dcterms "modified")}
                             body  (conj (str dcterms "description"))
                             title (conj (str rdfs "label")))
        attribs (merge (parse-attribs new-attribs) attribs)
        _ (info :post-attribs attribs)
        triples (->> attribs
                     (map (fn [[p o]] [id (name p) o]))
                     (concat triples)
                     (trio/triple-seq))]
    (prn :new-attribs attribs)
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
                  :prefixes (build-prefixes prefixes graph))))))
    (spit "graph.edn" (sort (trio/select (:graph @state))))
    {::id id}))

(defn describe-resource
  [id]
  (q/query
   {:describe '?s
    :from (:graph @state)
    :query [{:where '[[?s ?p ?o]]}]
    :values {'?s #{id}}}))

(defn inject-var
  [res x]
  (if (q/qvar? x)
    (let [[[_ k v]] (re-seq #"\?(\w+):(\w+)" (name x))]
      (get-in res [(keyword k) (symbol (str "?" v))]))
    x))

(defn expand-pnames-in-query
  [prefixes x]
  (if (string? x) (vu/expand-pname prefixes x) x))

(defn populate-template
  [prefixes graph this {:syms [?q ?tpl] :as spec}]
  (info :template-spec spec)
  (let [qspecs (read-string ?q)
        qspecs (postwalk #(expand-pnames-in-query prefixes %) qspecs)
        _ (info :expanded qspecs)
        qres   (reduce-kv
                (fn [acc k spec]
                  (let [spec (assoc spec :from graph :values {'?this #{this}})
                        res  (first (q/query spec))]
                    (assoc acc k res)))
                {} qspecs)]
    (info :tpl-res qres)
    (->> ?tpl
         read-string
         (postwalk #(inject-var qres %))
         seq)))

(defn build-resource-template
  [{:strs [ea rdf] :as prefixes} graph id]
  (->> {:select :*
        :from graph
        :query [{:where [[id (str rdf "type") '?type]
                         ['?type (str ea "hasTemplate") '?tpl-id]
                         ['?tpl-id (str ea "query") '?q]
                         ['?tpl-id (str ea "instanceView") '?tpl]]}]}
       q/query
       (map #(populate-template prefixes graph id %))))

(defmulti handle-resource-get #(-> % :representation :media-type))

(defmethod handle-resource-get "application/edn"
  [ctx]
  (let [id (maybe-number (-> ctx :request :params :id))]
    {:body (vec (describe-resource id))}))

(defmethod handle-resource-get "application/json"
  [ctx]
  (let [id (maybe-number (-> ctx :request :params :id))]
    (json/write-str
     {:body (vec (describe-resource id))}
     :escape-slash false)))

(defmethod handle-resource-get "text/html"
  [ctx]
  (let [{:keys [prefixes graph]} @state
        {:strs [dcterms rdfs this]} prefixes
        _ (info :prefixes prefixes)
        _ (info :dcterms dcterms)
        _ (info :rdfs rdfs)
        _ (info :this this)
        id (-> ctx :request :params :id)
        _ (info :id1 (vu/expand-pname prefixes id))
        id (if-let [uri (vu/expand-pname prefixes id)] uri (str this id))
        _ (info :id id (-> ctx :request :uri))
        id (maybe-number id)
        {:syms [?body ?title] :as res}
        (first
         (q/query
          {:select :*
           :from graph
           :query [{:where [[id (str dcterms "description") '?body]]}
                   {:union [[id (str rdfs "label") '?title]]}]}))
        template (build-resource-template prefixes graph id)
        attribs (q/query
                 {:select :*
                  :from graph
                  :query [{:where [[id '?att '?val]]
                           :filter {'?att #(not (#{(str dcterms "description") (str rdfs "label")} %))}}
                          {:optional [['?att (str rdfs "label") '?atitle]]}
                          {:optional [['?val (str rdfs "label") '?vtitle]]}]
                  :group '?att})
        shared-pred (q/query
                     {:select :*
                      :from graph
                      :query [{:where [['?other id '?val]]}
                              {:optional [['?other (str rdfs "label") '?otitle]]}
                              {:optional [['?val (str rdfs "label") '?vtitle]]}]
                      :order-asc '[?other ?val]})
        shared-obj (q/query
                    {:select :*
                     :from graph
                     :query [{:where [['?other '?pred id]]}
                             {:optional [['?other (str rdfs "label") '?otitle]]}
                             {:optional [['?pred (str rdfs "label") '?ptitle]]}]
                     :order-asc '[?other ?pred]})]
    ;;(info id :title ?title)
    ;;(info id :attribs attribs)
    ;;(info id :shared-p shared-pred)
    ;;(info id :shared-o shared-obj)
    (view/html-template
     [:div.row
      [:div.col-xs-12 [:h1 (or ?title id)]]]
     [:form {:method :post :action (str "/resources/" id)}
      [:div.row
       [:div.col-sm-8.col-md-9
        (view/content-tab-panels ?body template)
        (when (or (seq shared-pred) (seq shared-obj))
          (view/related-resource-table prefixes (or ?title id) shared-pred shared-obj))]
       (view/attrib-sidebar prefixes graph attribs (attrib-templates prefixes graph))]])))

(defresource resource [id]
  :available-media-types ["text/html" "application/edn" "application/json"]
  :allowed-methods [:get :post]
  :handle-ok handle-resource-get
  :post! handle-resource-update
  :post-redirect? (fn [ctx] {:location (str "/resources/" (::id ctx))}))

(defroutes app-routes
  (GET "/" [] (resp/redirect (str "/resources/this:Index")))
  (ANY ["/resources/:id" :id #".*"] [id] (resource id)))

(def app
  (->> (assoc-in site-defaults [:security :anti-forgery] false)
       (wrap-defaults app-routes)))
