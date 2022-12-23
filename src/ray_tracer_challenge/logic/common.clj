(ns ray-tracer-challenge.logic.common)

(def epsilon 0.00001)

(defn close-to-zero? [value] (< (abs value) epsilon))
