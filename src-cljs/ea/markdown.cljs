(ns ea.markdown
  (:require-macros
   [reagent.ratom :refer [reaction]]
   [cljs-log.core :refer [debug info warn]])
  (:require
   [ea.router :as router]
   [re-frame.core :refer [subscribe dispatch]]
   [clojure.string :as str]
   [reagent.core :as reagent]))

(def pname-regexp #"^([A-Za-z0-9\\-_]+):([A-Za-z0-9\\-_]*)$")
(def pname-title-regexp #"([A-Za-z0-9\\-_]+:[A-Za-z0-9\\-_]*)\|(.+)")

(def link-regexp
  (re-pattern
   (str "\\["
        "([A-Za-z0-9\\-_]+:[A-Za-z0-9\\-_]*)" ;; pname
        "?\\|?((\\w|\\s)+)\\]\\(" ;; title
        "([\\/A-Za-z0-9#%&\\:\\?_\\-]+)" ;; uri
        "\\)")))

(defn maybe-pname-resource-uri
  [pn]
  (if-let [parts (re-find pname-regexp pn)]
    (str "/resources/" (first parts))
    pn))

(defn pname-link-title
  [title]
  (if-let [parts (re-find pname-title-regexp title)]
    (peek parts)
    title))

(defn md-link-transformer
  [href title text]
  (let [href (maybe-pname-resource-uri href)
        text (or (pname-link-title text) text)]
    (str "<a href=\"" href "\"><span class=\"glyphicon glyphicon-new-window\"></span> " text "</a>")))

(def renderer
  (let [r (aget js/window "__EA_MD_RENDERER__")]
    (aset r "link" md-link-transformer)
    r))

(defn highlight-code [html-node]
  (let [nodes (.querySelectorAll html-node "pre code")]
    (loop [i (.-length nodes)]
      (when-not (neg? i)
        (when-let [item (.item nodes i)]
          (.highlightBlock js/hljs item))
        (recur (dec i))))))

(defn markdown-component [content]
  [(with-meta
     (fn []
       [:div
        {:dangerouslySetInnerHTML
         {:__html (-> content (str) (js/marked #js {:renderer renderer}))}}])
     {:component-did-mount
      (fn [this]
        (let [node (reagent/dom-node this)]
          (highlight-code node)))})])

(defn preview [content]
  (when (not-empty content)
    (markdown-component content)))

(defn editor [content]
  [:textarea.form-control
   {:value @content
    :on-change #(reset! content (-> % .-target .-value))}])

(defn page []
  (let [content (reagent/atom nil)]
    (fn []
      [:div
       [:h1 "Live Markdown Editor"]
       [:div.container-fluid
        [:div.row
         [:div.col-sm-6
          [:h3 "Editor"]
          [editor content]]
         [:div.col-sm-6
          [:h3 "Preview"]
          [preview content]]]]])))
