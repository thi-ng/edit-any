(ns ea.templates
  (:require-macros
   [cljs-log.core :refer [debug info warn]])
  (:require
   [ea.subs :as subs]
   [ea.markdown :as md]
   [thi.ng.trio.query :as q]
   [thi.ng.trio.vocabs.utils :as vu]
   [re-frame.core :refer [subscribe dispatch dispatch-sync]]
   [clojure.walk :refer [postwalk]]
   [cljs.reader :refer [read-string]]))

(defn inject-result-var
  [res x]
  (if (q/qvar? x)
    (let [[[_ k v]] (re-seq #"^\?([A-Za-z0-9\-_]+):([A-Za-z0-9\-_]+)$" (name x))
          qres (res (keyword k))
          qvar (symbol (str \? v))]
      (if (map? qres) (qres qvar) (some #(% qvar) qres)))
    x))

(defn expand-pname-in-query
  [prefixes x]
  (if (string? x)
    (or (vu/expand-pname prefixes x) x)
    x))

(defn form-field-attribs-common
  [attribs res]
  (-> attribs
      (assoc
       :key          (:value attribs)
       :defaultValue (inject-result-var (:tpl-results res) (:value attribs))
       :class        "form-control"
       :on-change    #(dispatch [:resource-field-edit (:name attribs) (-> % .-target .-value)]))
      (dissoc :value)))

(defmulti form-field (fn [f res] (info :transform f) (first f)))

(defmethod form-field :default [f _] f)

(defmethod form-field :input
  [f res]
  (update f 1 form-field-attribs-common res))

(defmethod form-field :textarea
  [[_ attribs :as f] res]
  (dispatch
     [:set-resource-field (:name attribs) (inject-result-var (:tpl-results res) (:value attribs))])
  (update f 1 form-field-attribs-common res))

(defmethod form-field :md-preview
  [f res]
  (let [[_ {src :of :as attribs}] f
        kid (keyword (str "md-" src))
        _ (subs/path-subscription kid [:resource-form src])
        content (subscribe [kid])]
    [(fn []
       [:div.form-group
        [:label "Preview"]
        (when @content
          [md/markdown-component @content])])]))

(defn form-field-transformer
  [res]
  (fn [x]
    (if (and (vector? x) (keyword? (first x)) (map? (second x)))
      (form-field x res)
      x)))

(defn build-resource-template
  [res]
  (let [spec (read-string (get-in res [:tpl '?tpl]))
        _    (info :tpl-spec spec)
        tpl (postwalk (form-field-transformer res) spec)]
    (assoc res :tpl tpl)))

