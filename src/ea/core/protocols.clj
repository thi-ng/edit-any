(ns ea.core.protocols)

(defprotocol IDBModel
  (prefix-map [_])
  (update-prefix-map [_])
  (graph [_])
  (update-graph [_ old new]))

(defprotocol IHandler
  (route-map [_]))
