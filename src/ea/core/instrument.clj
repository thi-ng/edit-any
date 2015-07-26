(ns ea.core.instrument)

(defmacro timed-action
  "Evaluates `body` in a do form and measures execution time.
  Returns result as 2-element vector: `[body-result time-in-msecs]`"
  [& body]
  `(let [t0# (System/nanoTime)]
     [(do ~@body) (* 1e-6 (- (System/nanoTime) t0#))]))
