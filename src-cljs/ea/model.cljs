(ns ea.model
  (:require
   [thi.ng.trio.vocabs.utils :as vu]))

(defn format-pname
  [[p n]] (str p ":" n))

(defn resource-uri
  [prefixes id]
  (let [uri?    (re-seq #"^(https?://|mailto:|ftp://)" id)
        pn      (vu/find-prefix prefixes id)
        pname   (if pn (format-pname pn))
        res-uri (if (and pn (= "this" (pn 0))) id (str (prefixes "this") (or pname id)))]
    [res-uri pname uri?]))
