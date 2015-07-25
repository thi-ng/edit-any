(ns ea.core.protocols)

(defprotocol IDBModel
  (prefix-map [_])
  (update-prefix-map [_])
  (graph [_]))

(defprotocol IHandler
  (route-map [_]))
