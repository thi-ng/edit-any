(ns ea.core.templates
  (:require
   [ea.core.model :as model :refer [ea]]
   [thi.ng.trio.core :as trio]
   [thi.ng.trio.query :as q]
   [thi.ng.trio.vocabs.utils :as vu]
   [thi.ng.trio.vocabs.rdf :refer [rdf]]
   [thi.ng.trio.vocabs.rdfs :refer [rdfs]]
   [thi.ng.trio.vocabs.dcterms :refer [dcterms]]
   [thi.ng.trio.vocabs.owl :refer [owl]]
   [clojure.walk :refer [postwalk]]
   [taoensso.timbre :refer [info warn error]]))

(defn inject-result-var
  [res x]
  (if (q/qvar? x)
    (let [[[_ k v]] (re-seq #"^?([A-Za-z0-9\-_]+):([A-Za-z0-9\-_]+)$" (name x))]
      (get-in res [(keyword k) (symbol (str \? v))]))
    x))

(defn expand-pname-in-query
  [prefixes x]
  (if (string? x)
    (or (vu/expand-pname prefixes x) x)
    x))

(defn populate-template
  [prefixes graph this {:syms [?q ?tpl] :as spec}]
  (info :template-spec spec)
  (let [qspecs (read-string ?q)
        qspecs (postwalk #(expand-pname-in-query prefixes %) qspecs)
        _ (info :expanded qspecs)
        qres   (reduce-kv
                (fn [acc k spec]
                  (let [spec (assoc spec :from graph :values {'?this #{this}})
                        res  (first (q/query spec))]
                    (assoc acc k res)))
                {} qspecs)]
    (info :tpl-res qres)
    (->> ?tpl
         (read-string)
         (postwalk #(inject-result-var qres %))
         (list))))

(defn build-resource-template
  [prefixes graph id]
  (->> {:select :*
        :from graph
        :query [{:where [[id (:type rdf) '?type]
                         ['?type (:hasTemplate ea) '?tpl-id]
                         ['?tpl-id (:query ea) '?q]
                         ['?tpl-id (:instanceView ea) '?tpl]]}]}
       (q/query)
       (map #(populate-template prefixes graph id %))
       (seq)))
