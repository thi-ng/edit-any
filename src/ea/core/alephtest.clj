(ns ea.core.alephtest
  (:require
   [manifold.stream :as s]
   [manifold.deferred :as d]
   [manifold.bus :as bus]
   [aleph.http :as http]
   [compojure.core :as compojure :refer [GET ANY]]
   [compojure.route :as route]
   [ring.util.response :as resp]
   [clojure.core.async :as async :refer [go go-loop <! >! close! chan timeout]]))

(defn hello-handler
  [req]
  (-> (resp/response "hello")
      (resp/content-type "text/html")))

(defn delayed-hello-handler
  [req]
  (d/->deferred
   (go
     (<! (timeout 2000))
     (hello-handler req))))

(defn streaming-numbers-handler
  [{:keys [params]}]
  (let [cnt (Integer/parseInt (get params :n "1"))
        body (chan)]
    (go-loop [i 0]
      (if (< i cnt)
        (let [_ (<! (timeout 100))]
          (>! body (str i "\n"))
          (recur (inc i)))
        (close! body)))
    {:status 200
     :headers {"content-type" "text/plain"}
     :body (s/->source body)}))

(defn non-ws-request
  []
  {:status 400
   :headers {"content-type" "application/text"}
   :body "Expected a websocket request."})

(defn ws-echo-handler
  [req]
  (-> (d/let-flow [socket (http/websocket-connection req)]
                  (s/connect socket socket))
      (d/catch non-ws-request)))

(def chatrooms (bus/event-bus))

(defn ws-chat-loop
  [conn]
  (d/let-flow
   [room (s/take! conn)
    name (s/take! conn)]
   (s/connect
    (bus/subscribe chatrooms room)
    conn)
   (s/consume
    #(bus/publish! chatrooms room %)
    (->> conn
         (s/map #(str name ": " %))
         (s/buffer 100)))))

(defn ws-chat-handler
  [req]
  (d/let-flow
   [conn (d/catch (http/websocket-connection req) (fn [_] nil))]
   (if-not conn
     non-ws-request
     (ws-chat-loop conn))))

(defn build-aleph-example-routes
  [config model]
  (compojure/routes
   (GET "/hello" [] hello-handler)
   (GET "/hello2" [] delayed-hello-handler)
   (GET "/count" [] streaming-numbers-handler)
   (GET "/ws" [] ws-echo-handler)
   (GET "/chat" [] ws-chat-handler)
   (route/not-found "404")))
