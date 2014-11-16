(ns ea.core.handler
  (:require
   [compojure.core :refer :all]
   [compojure.route :as route]
   [liberator.core :as lib :refer [defresource]]
   [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
   [thi.ng.trio.core :as trio]
   [thi.ng.trio.query :as q]
   [clojure.edn :as edn]
   [hiccup.page :refer [html5 include-js include-css]]
   [hiccup.element :as el]
   [markdown.core :as md]))

(def graph
  (->> "graph.edn"
       slurp
       edn/read-string
       (apply trio/plain-store)
       ref))

(def date-format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss"))

(defn format-date
  [d] (.format date-format d))

(defn format-timestamp
  [t] (.format date-format (java.util.Date. t)))

(defn resource-link
  [id & [label]]
  (let [id (if (keyword? id) (name id) id)]
    (str "<a href=\"/resources/" id "\">" (or label id) "</a>")))

(defn maybe-number
  [x] (try (Long/parseLong x) (catch Exception e x)))

(defresource page [id]
  :available-media-types ["text/html" "application/edn"]
  :allowed-methods [:get :post :patch]
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
                 (html5
                  [:head
                   (apply include-css ["/css/bootstrap.min.css"])
                   (apply include-js [ "/js/jquery-2.1.1.min.js" "/js/bootstrap.min.js" "/js/marked.min.js"])]
                  [:body
                   [:div.container
                    [:div.row
                     [:div.col-md-12
                      [:h1 (or ?title id)]
                      [:form {:method :post :action (str "/resources/" id)}
                       [:div {:role "tabpanel"}
                        [:ul.nav.nav-tabs {:role "tablist"}
                         [:li.active {:role "presentation"} [:a {:href "#preview" :role "tab" :data-toggle "tab"} "Preview"]]
                         [:li {:role "presentation"} [:a {:href "#edit" :role "tab" :data-toggle "tab"} "Edit"]]]
                        [:div.tab-content
                         [:div#preview.tab-pane.fade.in.active {:role "tabpanel"}
                          (md/md-to-html-string ?body)]
                         [:div#edit.tab-pane.fade {:role "tabpanel"}
                          [:p [:textarea#editor.form-control {:name "body" :rows 10} ?body]]
                          [:p [:button.btn.btn-primary {:type "submit"} "Submit"]]]]]
                       (when (seq attribs)
                         (list
                          [:h2 "Attribs"]
                          [:table.table.table-striped
                           (map
                            (fn [{:syms [?att ?atitle ?val ?vtitle]}]
                              [:tr
                               [:td (resource-link ?att ?atitle)]
                               [:td (resource-link ?val ?vtitle)]])
                            attribs)]))
                       (when (or (seq shared-pred) (seq shared-obj))
                         (list
                          [:h2 "Other resources"]
                          [:table.table.table-striped
                           (map
                            (fn [{:syms [?other ?otitle ?val ?vtitle]}]
                              [:tr
                               [:td (resource-link ?other ?otitle)]
                               [:td (or ?title id)]
                               [:td (resource-link ?val ?vtitle)]])
                            shared-pred)
                           (map
                            (fn [{:syms [?other ?otitle ?pred ?ptitle]}]
                              [:tr
                               [:td (resource-link ?other ?otitle)]
                               [:td (resource-link ?pred ?ptitle)]
                               [:td (or ?title id)]])
                            shared-obj)]))]]]]
                   (el/javascript-tag
                    "$(\"#editor\").blur(function(e){$(\"#preview\").html(marked(e.target.value));console.log(\"MD updated\");})")])))
  :post! (fn [ctx]
           (dosync
            (let [{:keys [body title attribs]} (get-in ctx [:request :params])
                  triples (cond-> [[id "dcterms:modified" (format-date (java.util.Date.))]]
                                  body (conj [id "dcterms:content" body])
                                  title (conj [id "rdfs:label" title]))
                  remove-props (cond-> #{"dcterms:modified"}
                                       body  (conj "dcterms:content")
                                       title (conj "rdfs:label"))
                  triples (->> attribs
                               (map (fn [[p o]] [id (name p) o]))
                               (concat triples)
                               (trio/triple-seq))]
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
                             (trio/add-triples triples)))))
              (spit "graph.edn" (sort (trio/select @graph)))))
           {::id id})
  :post-redirect? (fn [ctx] {:location (str "/resources/" (::id ctx))}))

(defroutes app-routes
  (ANY "/resources/:id" [id] (page id)))

(def app
  (->> (assoc-in site-defaults [:security :anti-forgery] false)
       (wrap-defaults app-routes)))
