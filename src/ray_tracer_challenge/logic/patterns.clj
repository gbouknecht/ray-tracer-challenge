(ns ray-tracer-challenge.logic.patterns
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]))

(defn pattern [transform pattern-at]
  {:transform  (or transform identity-matrix)
   :pattern-at pattern-at})
(defn pattern-at [pattern point] ((:pattern-at pattern) pattern point))
(defn pattern-at-shape [pattern shape world-point]
  (let [object-point (multiply-matrix-by-tuple (inverse (:transform shape)) world-point)
        pattern-point (multiply-matrix-by-tuple (inverse (:transform pattern)) object-point)]
    (pattern-at pattern pattern-point)))

(defn stripe-pattern [a b & {:keys [transform]}]
  (letfn [(pattern-at [pattern [x _ _]]
            (if (= (mod (int (Math/floor x)) 2) 0) (:a pattern) (:b pattern)))]
    (assoc (pattern transform pattern-at) :a a :b b)))
(defn gradient-pattern [a b & {:keys [transform]}]
  (letfn [(pattern-at [pattern [x _ _]]
            (let [distance (subtract-colors (:b pattern) (:a pattern))
                  fraction (- x (Math/floor x))]
              (add-colors (:a pattern) (multiply-color distance fraction))))]
    (assoc (pattern transform pattern-at) :a a :b b)))
(defn ring-pattern [a b & {:keys [transform]}]
  (letfn [(pattern-at [pattern [x _ z]]
            (if (= (mod (int (Math/floor (sqrt (+ (* x x) (* z z))))) 2) 0) (:a pattern) (:b pattern)))]
    (assoc (pattern transform pattern-at) :a a :b b)))
(defn checkers-pattern [a b & {:keys [transform]}]
  (letfn [(pattern-at [pattern [x y z]]
            (if (= (mod (int (+ (Math/floor x) (Math/floor y) (Math/floor z))) 2) 0) (:a pattern) (:b pattern)))]
    (assoc (pattern transform pattern-at) :a a :b b)))
