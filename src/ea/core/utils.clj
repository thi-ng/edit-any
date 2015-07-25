(ns ea.core.utils
  (:require
   [clojure.string :as str]
   [clj-time.core :as t]
   [clj-time.format :as tf]
   [markdown.transformers :as tx]
   [thi.ng.trio.vocabs.utils :as vu]))

(defn truncate
  [limit s]
  (if (and limit (> (count s) limit))
    (str (subs s 0 limit) "\u2026") ;; append ellipsis
    s))

(defn format-date
  [dt] (tf/unparse (tf/formatters :mysql) dt))

(defn line-endings
  [s] (str/replace s "\r\n" "\n"))

(defn md-rdfa-href
  [prefixes title link prop]
  (let [prop-uri (vu/expand-pname prefixes (str/join prop))
        link (vu/expand-pname-maybe prefixes (str/join link))]
    (tx/escape-link
     "<a href=\"" link "\" property=\"" prop-uri "\" title=\"" prop "\">" title "</a>")))

(defn md-link
  [prefixes]
  (fn [text {:keys [code codeblock] :as state}]
    (if (or code codeblock)
      [text state]
      (loop [out []
             tokens (seq text)]
        (if (empty? tokens)
          [(str/join out) state]

          (let [[head xs]   (split-with (partial not= \[) tokens)
                xs          (tx/handle-img-link xs)
                [title ys]  (split-with (partial not= \]) xs)
                [dud zs]    (split-with (partial not= \() ys)
                [link tail] (split-with (partial not= \)) zs)]

            (if (or (< (count link) 2)
                    (< (count tail) 1)
                    (> (count dud) 1))
              (recur (concat out head title dud link) tail)
              (recur
               (into out
                     (if (= (last head) \!)
                       (let [alt (rest title)
                             [url title] (split-with (partial not= \space) (rest link))
                             title (str/join (rest title))]
                         (concat (butlast head) (tx/img alt url title)))
                       (let [title (rest title)
                             [pn title] (split-with (partial not= \|) title)
                             [pn title] (if (seq title) [pn (rest title)] [nil pn])
                             link (rest link)]
                         (concat head (if pn
                                        (md-rdfa-href prefixes title link pn)
                                        (tx/href title link))))))
               (rest tail)))))))))
