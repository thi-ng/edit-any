(ns ea.core.handler
  (:require
   [compojure.core :refer :all]
   [compojure.route :as route]
   [liberator.core :as lib :refer [defresource]]
   [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
   [thi.ng.trio.core :as trio]
   [thi.ng.trio.query :as q]
   [clojure.pprint :refer [pprint]]))

(def date-format (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss.SSSZ"))

(defn format-timestamp
  [t] (.format date-format (java.util.Date. t)))

(defonce graph (ref (trio/plain-store)))

(defn resource-link
  [id & [label]]
  (let [id (if (keyword? id) (name id) id)]
    (str "<a href=\"/resources/" id "\">" (or label id) "</a>")))

(defn maybe-number
  [x] (try (Long/parseLong x) (catch Exception e x)))

(defresource page [id]
  :available-media-types ["text/html" "application/edn"]
  :allowed-methods [:get :post :put]
  :handle-ok (fn [ctx]
               (let [id (maybe-number id)
                     {:syms [?body ?title] :as res}
                     (first (q/query
                             {:select :*
                              :from @graph
                              :query [{:where [[id "dcterms:content" '?body]]}
                                      {:optional [[id "rdfs:label" '?title]]}]}))
                     attribs (q/query
                              {:select :*
                               :from @graph
                               :query [{:where [[id '?att '?val]]
                                        :filter {'?att #(not (#{"dcterms:content" "rdfs:label"} %))}}
                                       {:optional [['?att "rdfs:label" '?atitle]]}
                                       {:optional [['?val "rdfs:label" '?vtitle]]}]
                               :order-asc '[?att ?val]})
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
                                  :order-asc '[?other ?pred]})]
                 (str
                  "<h1>" (or ?title id) "</h1>"
                  "<textarea>" ?body "</textarea>"
                  (when (seq attribs)
                    (str "<h2>Attribs</h2>"
                         "<table>"
                         (->> attribs
                              (map (fn [{:syms [?att ?atitle ?val ?vtitle]}]
                                     (str "<tr><td>" (resource-link ?att ?atitle) "</td>"
                                          "<td>" (resource-link ?val ?vtitle) "</td></tr>")))
                              (apply str))
                         "</table>"))
                  (when (or (seq shared-pred) (seq shared-obj))
                    (str "<h2>Other resources</h2>"
                         "<table>"
                         (->> shared-pred
                              (map (fn [{:syms [?other ?otitle ?val ?vtitle]}]
                                     (str "<tr><td>" (resource-link ?other ?otitle) "</td>"
                                          "<td>" (or ?title id) "</td>"
                                          "<td>" (resource-link ?val ?vtitle) "</td></tr>")))
                              (apply str))
                         (->> shared-obj
                              (map (fn [{:syms [?other ?otitle ?pred ?ptitle]}]
                                     (str "<tr><td>" (resource-link ?other ?otitle) "</td>"
                                          "<td>" (resource-link ?pred ?ptitle) "</td>"
                                          "<td>" (or ?title id) "</td></tr>")))
                              (apply str))
                         "</table>")))))
  :post! (fn [ctx]
           (dosync
            (let [{:keys [body title attribs]} (get-in ctx [:request :params])
                  triples (->> (map (fn [[p o]] [id (name p) o]) attribs)
                               (concat [[id "dcterms:content" body]
                                        [id "rdfs:label" title]
                                        [id "dcterms:modified" (format-timestamp (System/currentTimeMillis))]]))]
              (prn :triples triples)
              (alter graph trio/add-triples triples)
              (spit "graph.edn" (sort (trio/select @graph))))))
  :post-redirect? (fn [ctx] {:location (str "/resources/" id)}))

(defroutes app-routes
  (ANY "/resources/:id" [id] (page id)))

(def app
  (wrap-defaults app-routes (assoc-in site-defaults [:security :anti-forgery] false)))
