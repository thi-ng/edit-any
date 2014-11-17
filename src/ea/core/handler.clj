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
   [clj-time.core :as t]
   [clj-time.format :as tf]
   [taoensso.timbre :refer [info warn error]]))

(def graph
  (ref
   (try
     (->> "graph.edn" slurp edn/read-string trio/as-model)
     (catch Exception e
       (->> "default-graph.edn" io/resource slurp edn/read-string trio/as-model)))))

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

(defn parse-attribs
  [src]
  (->> src
       (str/split-lines)
       (map
        #(let [s (.indexOf % "=")]
           (if (>= s 0) [(subs % 0 s) (subs % (inc s))])))
       (filter identity)
       (reduce
        (fn [acc [k v]] (update-in acc [k] (fnil conj []) v)) {})))

(defn handle-resource-update
  [ctx]
  (let [{:keys [id body title attribs new-attribs]} (get-in ctx [:request :params])
        now (t/now)
        triples (cond-> [[id "dcterms:modified" (format-date now)]]
                        body (conj [id "dcterms:description" body])
                        title (conj [id "rdfs:label" title]))
        remove-props (cond-> #{"dcterms:modified"}
                             body  (conj "dcterms:description")
                             title (conj "rdfs:label"))
        attribs (merge (parse-attribs new-attribs) attribs)
        _ (info :post-attribs attribs)
        triples (->> attribs
                     (map (fn [[p o]] [id (name p) o]))
                     (concat triples (date-triples id now))
                     (trio/triple-seq))]
    (prn :new-attribs new-attribs)
    (dosync
     (alter graph
            (fn [g]
              (let [old (q/query
                         {:construct '[[?s ?p ?o]]
                          :from g
                          :query [{:where [['?s '?p '?o]]}]
                          :values {'?s #{id} '?p remove-props}})]
                ;;(prn :old old)
                ;;(prn :new triples)
                (-> g
                    (trio/remove-triples old)
                    (trio/add-triples triples))))))
    (spit "graph.edn" (sort (trio/select @graph)))
    {::id id}))

(defn describe-resource
  [id]
  (q/query
   {:describe '?s
    :from @graph
    :query [{:where '[[?s ?p ?o]]}]
    :values {'?s #{id}}}))

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
  (let [id (maybe-number (-> ctx :request :params :id))
        {:syms [?body ?title] :as res}
        (first (q/query
                {:select :*
                 :from @graph
                 :query [{:where [[id "dcterms:description" '?body]]}
                         {:union [[id "rdfs:label" '?title]]}]}))
        attribs (q/query
                 {:select :*
                  :from @graph
                  :query [{:where [[id '?att '?val]]
                           :filter {'?att #(not (#{"dcterms:description" "rdfs:label"} %))}}
                          {:optional [['?att "rdfs:label" '?atitle]]}
                          {:optional [['?val "rdfs:label" '?vtitle]]}]
                  :group '?att})
        shared-pred (q/query
                     {:select :*
                      :from @graph
                      :query [{:where [['?other id '?val]]}
                              {:optional [['?other "rdfs:label" '?otitle]]}
                              {:optional [['?val "rdfs:label" '?vtitle]]}]
                      :order-asc '[?other ?val]})
        shared-obj (q/query
                    {:select :*
                     :from @graph
                     :query [{:where [['?other '?pred id]]}
                             {:optional [['?other "rdfs:label" '?otitle]]}
                             {:optional [['?pred "rdfs:label" '?ptitle]]}]
                     :order-asc '[?other ?pred]})
        media-type (get-in ctx [:representation :media-type])]
    (info :title ?title)
    (info :attribs attribs)
    (info :shared-p shared-pred)
    (info :shared-o shared-obj)
    (view/html-template
     [:div.row
      [:div.col-sm-3.col-lg-2] [:div.col-sm-9.col-lg-10 [:h1 (or ?title id)]]]
     [:form {:method :post :action (str "/resources/" id)}
      [:div.row
       (view/attrib-sidebar @graph attribs)
       [:div.col-sm-9.col-lg-10
        (view/content-tab-panels ?body)
        (when (or (seq shared-pred) (seq shared-obj))
          (view/related-resource-table (or ?title id) shared-pred shared-obj))]]])))

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
