(ns ray-tracer-challenge.logic.rays
  (:require [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn ray [origin direction] {:origin origin :direction direction})
(defn position [ray t] (add-tuples (:origin ray) (multiply-tuple (:direction ray) t)))
(defn transform [ray matrix]
  {:origin    (multiply-matrix-by-tuple matrix (:origin ray))
   :direction (multiply-matrix-by-tuple matrix (:direction ray))})
