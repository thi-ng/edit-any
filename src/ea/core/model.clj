(ns ea.core.model
  (:require
   [ea.core.protocols :as proto]
   [ea.core.utils :as utils]
   [thi.ng.trio.core :as trio]
   [thi.ng.trio.query :as q]
   [thi.ng.trio.vocabs :refer [defvocab]]
   [thi.ng.trio.vocabs.utils :as vu]
   [thi.ng.trio.vocabs.rdf :refer [rdf]]
   [thi.ng.trio.vocabs.rdfs :refer [rdfs]]
   [thi.ng.trio.vocabs.dcterms :refer [dcterms]]
   [thi.ng.trio.vocabs.owl :refer [owl]]
   [thi.ng.xerror.core :as err]
   [clj-time.core :as t]
   [clojure.data :refer [diff]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.core.async :as async]
   [clojure.walk :refer [postwalk]]
   [clojure.pprint :refer [pprint]]
   [com.stuartsierra.component :as comp]
   [taoensso.timbre :refer [info warn error]]))

(def default-prefixes
  {"ea"  "http://thi.ng/owl/edit-any#"
   "owl" "http://www.w3.org/2002/07/owl#"
   "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"})

(defvocab ea "http://thi.ng/owl/edit-any#"
  :AttributeCollection
  :hasTemplate
  :instanceView
  :prefix
  :query)

(defn build-prefixes
  [graph]
  (->> (q/query
        {:select :*
         :from   graph
         :query  [{:where [['?vocab (:type rdf) (:Description rdf)]
                           ['?vocab (:about rdf) '?uri]
                           ['?vocab (:prefix ea) '?prefix]
                           ['?uri (:type rdf) (:Ontology owl)]]}]})
       (reduce (fn [acc {:syms [?prefix ?uri]}] (assoc acc ?prefix ?uri)) {})))

(defn load-default-graph
  [path]
  (info "loading default graph:" path)
  (let [{:keys [prefixes triples]} (->> io/resource vu/load-vocab-triples)]
    (info "default graph size:" (count triples))
    {:graph (trio/as-model triples) :prefixes prefixes}))

(defn make-graph-writer
  [path]
  (let [ch (async/chan 16)]
    (async/go-loop []
      (let [g (async/<! ch)]
        (when g
          (info "writing graph: " path "triples:" (count g))
          (try
            (spit path (pr-str g))
            (catch Exception e (warn e "couldn't write graph")))
          (recur))))
    ch))

(defrecord MemoryDB [config state write-chan]
  comp/Lifecycle
  (start
    [_]
    (let [path (:path config)]
      (info "starting trio memstore using: " path)
      (->> (try
             (info "attempt reading graph...")
             (let [graph (->> path slurp (edn/read-string {:readers *data-readers*}) trio/as-model)]
               (info "graph size:" (trio/model-size graph))
               {:graph graph :prefixes (build-prefixes graph)})
             (catch Exception e
               (warn e)
               (load-default-graph (:default-path config))))
           (ref)
           (assoc _ :write-chan (make-graph-writer path) :state))))
  (stop
    [_]
    (info "stopping trio memstore...")
    (if write-chan
      (do (info "closing write channel")
          (async/close! write-chan)
          (assoc _ :write-chan nil))
      _))

  proto/IDBModel
  (prefix-map [_] (:prefixes @state))
  (update-prefix-map [_]
    (dosync
     (alter state assoc :prefixes (build-prefixes (:graph @state))))
    _)
  (graph [_] (:graph @state))
  (update-graph [_ old new]
    (dosync
     (alter state
            (fn [state]
              (let [graph    (-> (:graph state)
                                 (trio/remove-triples old)
                                 (trio/add-triples new))
                    prefixes (build-prefixes graph)]
                (assoc state
                       :prefixes prefixes
                       :graph graph)))))
    (info "updated graph, new triples: " new)
    (async/go (async/>! write-chan (trio/select (:graph @state))))
    _))

(defn make-db
  [db-conf]
  (condp = (:type db-conf)
    :memory (MemoryDB. db-conf nil nil)
    (err/unsupported! (str "Unsupported DB type: " (:type db-conf)))))

(defn format-pname
  [[p n]] (str p ":" n))

(defn all-predicates
  [model] (trio/predicates (proto/graph model)))

(defn all-resource-triples
  [model uri] (trio/select (proto/graph model) uri nil nil))

(defn as-resource-triple-seq
  [uri attribs]
  (->> attribs
       (map (fn [[p o]] [uri (name p) o]))
       (trio/triple-seq)))

(defn resource-uri ;; TODO old
  [prefixes id]
  (let [uri?    (re-seq #"^(https?://|mailto:|ftp://)" id)
        pn      (vu/find-prefix prefixes id)
        pname   (if pn (format-pname pn))
        res-uri (if (and pn (= "this" (pn 0))) id (str (prefixes "this") (or pname id)))]
    [res-uri pname uri?]))

(defn canonical-resource-uri
  [model uri]
  (let [prefixes (proto/prefix-map model)
        uri?     (re-seq #"^(https?://|mailto:|ftp://)" uri)
        pn       (vu/find-prefix prefixes uri)
        pname    (if pn (format-pname pn))
        res-uri  (if (and pn (= "this" (pn 0))) uri (str (prefixes "this") (or pname uri)))]
    [res-uri pname uri?]))

(defn as-resource-uri
  [model id]
  (let [prefixes (proto/prefix-map model)]
    (if-let [uri (vu/expand-pname prefixes id)]
      uri
      (if (re-find #"^(https?|ftp|mailto):" id)
        id
        (str (prefixes "this") id)))))

(defn resource-title-body
  [model uri]
  (-> {:select :*
       :from   (proto/graph model)
       :query  [{:where [[uri (:description dcterms) '?body]]}
                {:union [[uri (:label rdfs) '?title]]}]}
      (q/query)
      (q/accumulate-result-vars)))

(defn resource-title
  [model res uri]
  (or (first (res '?title))
      (if-let [pn (vu/find-prefix (proto/prefix-map model) uri)]
        (if (= "this" (pn 0)) (if (seq (pn 1)) (pn 1)) (format-pname pn)))))

(defn other-resource-attribs
  [model id]
  (q/query
   {:select :*
    :from   (proto/graph model)
    :query  [{:where [[id '?att '?val]]
              :filter {'?att #(not (#{(:description dcterms) (:label rdfs)} %))}}
             {:optional [['?att (:label rdfs) '?atitle]]}
             {:optional [['?val (:label rdfs) '?vtitle]]}]
    :group  '?att}))

(defn facet-shared-predicate
  [model id]
  (q/query
   {:select    :*
    :from      (proto/graph model)
    :query     [{:where [['?other id '?val]]}
                {:optional [['?other (:label rdfs) '?otitle]]}
                {:optional [['?val (:label rdfs) '?vtitle]]}]
    :order-asc '[?other ?val]}))

(defn facet-shared-object
  [model id]
  (q/query
   {:select    :*
    :from      (proto/graph model)
    :query     [{:where [['?other '?pred id]]}
                {:optional [['?other (:label rdfs) '?otitle]]}
                {:optional [['?pred (:label rdfs) '?ptitle]]}]
    :order-asc '[?other ?pred]}))

;; TODO cache results
(defn all-attrib-templates
  [model]
  (q/query
   {:select [{:id {:use '?id :fn #(format-pname (vu/find-prefix (proto/prefix-map model) %))}}
             {:tpl '?tpl}]
    :from   (proto/graph model)
    :query  [{:where [['?id (:type rdf) (:AttributeCollection ea)]
                      ['?id (:description dcterms) '?tpl]]}]}))

(defn resource-template
  [model id]
  (-> {:select :*
       :from   (proto/graph model)
       :query  [{:where [[id (:type rdf) '?type]
                         ['?type (:hasTemplate ea) '?tpl-id]
                         ['?tpl-id (:query ea) '?q]
                         ['?tpl-id (:instanceView ea) '?tpl]]}]}
      (q/query)
      (first)))

(defn expand-pname-in-query
  [prefixes x]
  (if (string? x)
    (or (vu/expand-pname prefixes x) x)
    x))

(defn execute-resource-tpl-queries
  [model uri {:syms [?q ?tpl] :as tpl-spec}]
  (info :template-spec tpl-spec)
  (let [prefixes (proto/prefix-map model)
        graph    (proto/graph model)
        qspecs   (read-string ?q)
        qspecs   (postwalk #(expand-pname-in-query prefixes %) qspecs)]
    (info :expanded qspecs)
    (reduce-kv
     (fn [acc k spec]
       (->> (assoc spec :from graph :values {'?this #{uri}})
            (q/query)
            (assoc acc k)))
     {} qspecs)))

(defn replace-triples
  [src new]
  (reduce
   (fn [acc t]
     (let [p (nth t 1)]
       (info :triple (trio/triple t) :p p)
       (if-let [del (->> src
                         (filter #(and (not= t %) (= p (:p %))))
                         (seq))]
         (let [acc (apply disj acc del)]
           (info :delete del)
           (if-not (empty? (last t)) (conj acc t) acc))
         (if-not (empty? (last t)) (conj acc t) acc))))
   (set src) new))

(defn replace-triples2
  [src new]
  (let [[del add keep] (diff (set src) new)]
    (info :delete del)
    (info :add add)
    (info :keep keep)
    add))

(def link-regexp
  (re-pattern
   (str "\\["
        "([A-Za-z0-9\\-_]+:[A-Za-z0-9\\-_]*)" ;; pname
        "?\\|?((\\w|\\s)+)\\]\\(" ;; title
        "([\\/A-Za-z0-9#%&\\:\\?_\\-]+)" ;; uri
        "\\)")))

(defn parse-links
  [txt]
  (->> txt
       (re-seq link-regexp)
       (reduce
        (fn [acc [_ pname title _ uri]]
          (if pname
            (update acc pname (fnil conj [])
                    (if (= \/ (first uri))
                      (str "http://localhost:3000" uri)
                      uri))
            acc))
        {})))

(defn compute-resource-changeset
  [model id attribs]
  (let [now      (t/now)
        fnow     (utils/format-date now)
        prefixes (proto/prefix-map model)
        uri      (if-let [uri (vu/expand-pname prefixes id)]
                   uri
                   (if (re-find #"^(https?|ftp|mailto):" id)
                     id
                     (str (prefixes "this") id)))
        attribs  (merge-with
                  #(into (if (vector? %) % [%]) %2)
                  attribs
                  ;;(parse-attribs bulk-attribs)
                  (if-let [desc (and attribs (attribs "dcterms:description"))]
                    (let [links (parse-links desc)]
                      (info :link links)
                      links)))
        attribs  (reduce-kv
                  (fn [acc p o]
                    (if-let [puri (vu/expand-pname prefixes p)]
                      (assoc
                       acc puri
                       (map #(or (vu/expand-pname prefixes %)
                                 (utils/line-endings %))
                            (if (vector? o) o [o])))
                      acc))
                  {} attribs)
        src      (all-resource-triples model uri)
        new      (as-resource-triple-seq uri attribs)
        auto     (if (nil? (seq src))
                   [[uri (:created dcterms) fnow]]
                   [[uri (:modified dcterms) fnow]])
        merged   (replace-triples2 src auto)
        merged   (if true
                   (replace-triples2 merged new)
                   (->> (into merged new)
                        (filter #(not (empty? (last %))))))
        delta    (diff (set src) merged)]
    ;;(info "------- src")
    ;;(pprint src)
    ;;(info "------- new")
    ;;(pprint new)
    ;;(info "------- merged")
    ;;(pprint merged)
    delta))

;;;;;;;;;;;;;;;;;; ooooollllldddddd

(defonce state (ref {}))

(defn init!
  []
  (try
    (let [graph    (->> "graph.edn" slurp edn/read-string trio/as-model)
          prefixes (build-prefixes graph)]
      (dosync (alter state (constantly {:prefixes prefixes :graph graph}))))
    (catch Exception e
      (let [{:keys [prefixes triples]}
            (->> "default-graph.edn"
                 io/resource
                 vu/load-vocab-triples)]
        (dosync
         (alter state
                (constantly {:prefixes prefixes :graph (trio/as-model triples)})))))))

(defn update!
  [old new]
  (->> (dosync
        (alter state
               (fn [{:keys [graph] :as state}]
                 (let [graph (-> graph
                                 (trio/remove-triples old)
                                 (trio/add-triples new))
                       prefixes (build-prefixes graph)]
                   (prn :new-prefixes prefixes)
                   (assoc state
                          :prefixes prefixes
                          :graph graph)))))
       (:graph)
       (trio/select)
       (sort)
       (spit "graph.edn")))

(defn new-resource?
  [id] (nil? (seq (trio/select (:graph @state) id nil nil))))

(defn describe-resource
  [id]
  (q/query
   {:describe '?s
    :from (:graph @state)
    :query [{:where '[[?s ?p ?o]]}]
    :values {'?s #{id}}}))

(defn get-attrib-templates
  [prefixes graph]
  (info :get-attrib-templates)
  (q/query
   {:select [{:id {:use '?id :fn #(format-pname (vu/find-prefix prefixes %))}}
             {:tpl '?tpl}]
    :from graph
    :query [{:where [['?id (:type rdf) (:AttributeCollection ea)]
                     ['?id (:description dcterms) '?tpl]]}]}))

(defn get-resource-description
  [graph id]
  (->> {:select :*
        :from graph
        :query [{:where [[id (:description dcterms) '?body]]}
                {:union [[id (:label rdfs) '?title]]}]}
       (q/query)
       (q/accumulate-result-vars)))
