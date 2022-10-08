(ns ray-tracer-challenge.logic.tuples
  (:require [clojure.math.numeric-tower :refer [sqrt]]))

(defn point [x y z] [x y z 1.0])
(defn point? [[_ _ _ w]] (= w 1.0))
(defn vektor [x y z] [x y z 0.0])
(defn vektor? [[_ _ _ w]] (= w 0.0))
(defn add-tuples [[x1 y1 z1 w1] [x2 y2 z2 w2]] [(+ x1 x2) (+ y1 y2) (+ z1 z2) (+ w1 w2)])
(defn subtract-tuples [[x1 y1 z1 w1] [x2 y2 z2 w2]] [(- x1 x2) (- y1 y2) (- z1 z2) (- w1 w2)])
(defn negate-tuple [[x y z w]] [(- x) (- y) (- z) (- w)])
(defn multiply-tuple [[x y z w] factor] [(* x factor) (* y factor) (* z factor) (* w factor)])
(defn divide-tuple [[x y z w] factor] [(/ x factor) (/ y factor) (/ z factor) (/ w factor)])
(defn magnitude-vektor [[x y z w]] (sqrt (+ (* x x) (* y y) (* z z) (* w w))))
(defn normalize-vektor [[x y z w :as vektor]]
  (let [m (magnitude-vektor vektor)]
    [(/ x m) (/ y m) (/ z m) (/ w m)]))
(defn dot-product-tuples [[x1 y1 z1 w1] [x2 y2 z2 w2]] (+ (* x1 x2) (* y1 y2) (* z1 z2) (* w1 w2)))
(defn cross-product-vektors [[x1 y1 z1] [x2 y2 z2]]
  (vektor (- (* y1 z2) (* z1 y2))
          (- (* z1 x2) (* x1 z2))
          (- (* x1 y2) (* y1 x2))))
