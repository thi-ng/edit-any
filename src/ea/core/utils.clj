(ns ea.core.utils
  (:require
   [clojure.string :as str]
   [clj-time.core :as t]
   [clj-time.format :as tf]))

(defn truncate
  [limit s]
  (if (and limit (> (count s) limit))
    (str (subs s 0 limit) "\u2026")
    s))

(defn format-date
  [dt] (tf/unparse (tf/formatters :mysql) dt))

(defn line-endings
  [s] (str/replace s "\r\n" "\n"))
