(ns ea.router
  (:require-macros
   [cljs-log.core :refer [debug info warn]])
  (:require
   [thi.ng.validate.core :as v]
   [clojure.string :as str]
   [goog.events :as events]
   [goog.history.EventType :as EventType])
  (:import
   [goog.history Html5History]))

(defonce history (doto (Html5History.) (.setUseFragment false)))

(defn format-route
  [route params]
  (->>  (:match route)
        (reduce
         (fn [acc x] (conj acc (if (keyword? x) (params x) x)))
         [])
        (str/join "/")))

(defn route-for-id
  [routes id args]
  (if-let [route (some #(if (= id (:id %)) %) routes)]
    (format-route route args)))

(defn match-route*
  [curr route]
  (if (= (count curr) (count route))
    (reduce
     (fn [acc [a b]]
       (cond
         (= a b) acc
         (keyword? b) (assoc acc b a)
         :else (reduced nil)))
     {} (partition 2 (interleave curr route)))))

(defn coerce-route-params
  [specs params]
  (reduce
   (fn [params [k {:keys [coerce]}]]
     (if coerce
       (if-let [pv (try (coerce (params k)) (catch js/Error e))]
         (assoc params k pv)
         (reduced nil))
       params))
   params specs))

(defn validate-route-params
  [specs params]
  (if-let [params (coerce-route-params specs params)]
    (let [valspecs (filter #(comp :validate val) specs)]
      (if (seq valspecs)
        (let [[params err] (->> valspecs
                                (reduce #(assoc % (key %2) (:validate (val %2))) {})
                                (v/validate params))]
          (if-not err params))
        params))))

(defn match-route
  [routes curr user]
  (some
   (fn [{:keys [match auth validate] :as spec}]
     (info :match match curr)
     (if (or user (not auth))
       (if-let [params (match-route* curr match)]
         (if-let [params (if validate (validate-route-params validate params) params)]
           (assoc spec :params params)))))
   routes))

(defn split-token
  [token]
  (let [items (str/split token "/")]
    (if-let [i (some (fn [[i x]] (if (#{"http:" "https:"} x) i)) (map-indexed vector items))]
      (concat (take i items) [(str/join "/" (drop i items))])
      items)))

(defn start!
  [routes default-route default-uri dispatch-route user]
  (info "starting router...")
  (doto history
    (events/listen
     EventType/NAVIGATE
     (fn [e]
       (let [token  (if-let [init-route (aget js/window "__EA_ROUTE__")]
                      (do
                        (aset js/window "__EA_ROUTE__" nil)
                        (.setToken history init-route)
                        init-route)
                      (.-token e))
             route  (split-token token)
             route' (match-route routes route @user)]
         (debug :route route :token token :id (:id route'))
         (if route'
           (dispatch-route route')
           (do
             (.setToken history default-uri)
             (dispatch-route default-route))))))
    (.setEnabled true)))

(defn trigger!
  [route]
  (.setToken history route))
