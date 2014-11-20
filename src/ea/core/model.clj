(ns ea.core.model
  (:require
   [ea.core.view :as view]
   [thi.ng.trio.core :as trio]
   [thi.ng.trio.query :as q]
   [thi.ng.trio.vocabs :refer [defvocab]]
   [thi.ng.trio.vocabs.utils :as vu]
   [thi.ng.trio.vocabs.rdf :refer [rdf]]
   [thi.ng.trio.vocabs.rdfs :refer [rdfs]]
   [thi.ng.trio.vocabs.dcterms :refer [dcterms]]
   [thi.ng.trio.vocabs.owl :refer [owl]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
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

(def state (ref {}))

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
        (dosync (alter state (constantly {:prefixes prefixes :graph (trio/as-model triples)})))))))

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
   {:select [{:id {:use '?id :fn #(view/fmt-pname (vu/find-prefix prefixes %))}}
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
       (first)))

(defn get-other-resource-attribs
  [graph id]
  (q/query
   {:select :*
    :from graph
    :query [{:where [[id '?att '?val]]
             :filter {'?att #(not (#{(:description dcterms) (:label rdfs)} %))}}
            {:optional [['?att (:label rdfs) '?atitle]]}
            {:optional [['?val (:label rdfs) '?vtitle]]}]
    :group '?att}))

(defn get-shared-predicate
  [graph id]
  (q/query
   {:select :*
    :from graph
    :query [{:where [['?other id '?val]]}
            {:optional [['?other (:label rdfs) '?otitle]]}
            {:optional [['?val (:label rdfs) '?vtitle]]}]
    :order-asc '[?other ?val]}))

(defn get-shared-object
  [graph id]
  (q/query
   {:select :*
    :from graph
    :query [{:where [['?other '?pred id]]}
            {:optional [['?other (:label rdfs) '?otitle]]}
            {:optional [['?pred (:label rdfs) '?ptitle]]}]
    :order-asc '[?other ?pred]}))
