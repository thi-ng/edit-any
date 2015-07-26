(ns ea.utils
  (:require-macros
   [cljs-log.core :refer [debug info warn]])
  (:require
   [thi.ng.domus.io :as io]
   [clojure.string :as str]
   [cljs.reader :refer [read-string]]
   [re-frame.core :refer [dispatch]]))

(defn safe-read-string
  [str]
  (try (read-string str) (catch js/Error e)))

(def keywordize (map #(if (number? %) % (keyword %))))

(def stringify (map #(if (number? %) % (name %))))

(defn truncate
  [limit s]
  (if (and limit (> (count s) limit))
    (str (subs s 0 limit) "\u2026") ;; append ellipsis
    s))

(defn assoc-in*
  "Like clojure.core/assoc-in only defaults to creation of
  intermediate vectors if a missing key is numeric."
  [m [k & ks] v]
  (if ks
    (assoc m k (assoc-in* (get m k (if (number? (first ks)) [] {})) ks v))
    (assoc m k v)))

;; from: clj-http.client
(defn dissoc*
  [m k]
  (if (vector? m)
    (vec (concat (if (> k 0) (subvec m 0 k)) (subvec m (inc k))))
    (dissoc m k)))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning
  a new nested structure. keys is a sequence of keys. Any empty maps
  that result will not be present in the new structure. Updated to
  work with vectors at any intermediate path position."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc* m k)))
      m)
    (dissoc* m k)))

(defn aget-in
  [obj path]
  (loop [obj obj, path (str/split path ".")]
    (if path
      (recur (aget obj (first path)) (next path))
      obj)))

(defn prevent [f]
  (fn [e] (.preventDefault e) (f e)))

(defn prevent->value [f]
  (prevent (fn [e] (f (-> e .-target .-value)))))

(defn do-request
  [spec]
  (io/request
   (merge
    {:method  :get
     :headers {"X-CSRF-token" (aget js/window "__X_TOKEN__")}
     :error   (fn [status data]
                (info :error-status status)
                (let [msg (if (-> @re-frame.db.app-db :session :user)
                            (str data " - Maybe reload the page?")
                            data)]
                  (dispatch
                   [:add-flash-msg
                    {:type :danger
                     :body [:span [:strong "Error: "] msg " (status: " status ")"]}])))}
    spec)))
