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
  [graph]
  (->> (q/query
        {:select :*
         :from graph
         :query [{:where [['?p "rdf:type" "rdf:Description"]
                          ['?p "rdf:about" '?uri]
                          ['?uri "rdf:type" "owl:Ontology"]]}]})
       (reduce (fn [acc {:syms [?p ?uri]}] (assoc acc ?p ?uri)) {})))

(defn start!
  []
  (let [graph (try
                (->> "graph.edn" slurp edn/read-string trio/as-model)
                (catch Exception e
                  (->> "default-graph.edn" io/resource slurp edn/read-string trio/as-model)))]
    (dosync
     (alter
      state
      (constantly
       {:prefixes (build-prefixes graph)
        :graph    graph})))))

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
  [graph]
  (q/query
   {:select [{:id '?id} {:tpl '?tpl}]
    :from graph
    :query [{:where '[[?id "rdf:type" "ea:AttributeCollection"]
                      [?id "dcterms:description" ?tpl]]}]}))
(defn new-resource?
  [id] (nil? (seq (trio/select (:graph @state) (maybe-number id) nil nil))))

(defn handle-resource-update
  [ctx]
  (let [{:keys [id body title attribs new-attribs replace]} (get-in ctx [:request :params])
        now (t/now)
        fnow (format-date now)
        triples (cond-> [[id "dcterms:modified" fnow]]
                        (new-resource? id) (conj [id "dcterms:created" fnow])
                        body               (conj [id "dcterms:description" body])
                        title              (conj [id "rdfs:label" title]))
        remove-props (cond-> #{"dcterms:modified"}
                             body  (conj "dcterms:description")
                             title (conj "rdfs:label"))
        attribs (merge (parse-attribs new-attribs) attribs)
        _ (info :post-attribs attribs)
        triples (->> attribs
                     (map (fn [[p o]] [id (name p) o]))
                     (concat triples)
                     (trio/triple-seq))]
    (prn :new-attribs attribs)
    (dosync
     (alter state
            (fn [{:keys [graph] :as state}]
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
                  :prefixes (build-prefixes graph))))))
    (spit "graph.edn" (sort (trio/select (:graph @state))))
    {::id id}))

(defn describe-resource
  [id]
  (q/query
   {:describe '?s
    :from (:graph @state)
    :query [{:where '[[?s ?p ?o]]}]
    :values {'?s #{id}}}))

(defn populate-template
  [graph this {:syms [?q ?tpl] :as spec}]
  (info :template-spec spec)
  (let [qspecs (read-string ?q)
        qres   (reduce-kv
                (fn [acc k spec]
                  (let [spec (assoc spec :from graph :values {'?this #{this}})
                        res  (first (q/query spec))]
                    (assoc acc k res)))
                {} qspecs)]
    (info :tpl-res qres)
    (->> ?tpl
         read-string
         (postwalk
          (fn [x]
            (if (q/qvar? x)
              (let [[[_ k v]] (re-seq #"\?(\w+):(\w+)" (name x))]
                (get-in qres [(keyword k) (symbol (str "?" v))]))
              x)))
         seq
         ((fn [t] (info :tpl t) t)))))

(defn build-resource-template
  [graph id]
  (->> {:select :*
        :from graph
        :query [{:where [[id "rdf:type" '?type]
                         ['?type "ea:hasTemplate" '?tpl-id]
                         ['?tpl-id "ea:query" '?q]
                         ['?tpl-id "ea:instanceView" '?tpl]]}]}
       q/query
       (map #(populate-template graph id %))))

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
        id (maybe-number (-> ctx :request :params :id))
        {:syms [?body ?title] :as res}
        (first
         (q/query
          {:select :*
           :from graph
           :query [{:where [[id "dcterms:description" '?body]]}
                   {:union [[id "rdfs:label" '?title]]}]}))
        template (build-resource-template graph id)
        attribs (q/query
                 {:select :*
                  :from graph
                  :query [{:where [[id '?att '?val]]
                           :filter {'?att #(not (#{"dcterms:description" "rdfs:label"} %))}}
                          {:optional [['?att "rdfs:label" '?atitle]]}
                          {:optional [['?val "rdfs:label" '?vtitle]]}]
                  :group '?att})
        shared-pred (q/query
                     {:select :*
                      :from graph
                      :query [{:where [['?other id '?val]]}
                              {:optional [['?other "rdfs:label" '?otitle]]}
                              {:optional [['?val "rdfs:label" '?vtitle]]}]
                      :order-asc '[?other ?val]})
        shared-obj (q/query
                    {:select :*
                     :from graph
                     :query [{:where [['?other '?pred id]]}
                             {:optional [['?other "rdfs:label" '?otitle]]}
                             {:optional [['?pred "rdfs:label" '?ptitle]]}]
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
       (view/attrib-sidebar prefixes graph attribs (attrib-templates graph))]])))

(defresource resource [id]
  :available-media-types ["text/html" "application/edn" "application/json"]
  :allowed-methods [:get :post]
  :handle-ok handle-resource-get
  :post! handle-resource-update
  :post-redirect? (fn [ctx] {:location (str "/resources/" (::id ctx))}))

(defroutes app-routes
  (GET "/" [] (resp/redirect "/resources/Index"))
  (ANY ["/resources/:id" :id #".*"] [id] (resource id)))

(def app
  (->> (assoc-in site-defaults [:security :anti-forgery] false)
       (wrap-defaults app-routes)))

(start!)
