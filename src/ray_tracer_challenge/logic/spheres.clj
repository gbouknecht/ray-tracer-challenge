(ns ray-tracer-challenge.logic.spheres
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn sphere [& {:keys [transform material]
                 :or   {transform identity-matrix
                        material  (material)}}]
  {:uuid      (random-uuid)
   :transform transform
   :material  material})
(defn normal-at [sphere world-point]
  (let [object-point (multiply-matrix-by-tuple (inverse (:transform sphere)) world-point)
        object-normal (subtract-tuples object-point (point 0 0 0))
        [wnx wny wnz] (multiply-matrix-by-tuple (transpose (inverse (:transform sphere))) object-normal)
        world-normal (vektor wnx wny wnz)]
    (normalize-vektor world-normal)))
(defn intersect [sphere ray]
  (let [ray (transform ray (inverse (:transform sphere)))
        sphere-to-ray (subtract-tuples (:origin ray) (point 0 0 0))
        a (dot-product-tuples (:direction ray) (:direction ray))
        b (* (dot-product-tuples (:direction ray) sphere-to-ray) 2)
        c (dec (dot-product-tuples sphere-to-ray sphere-to-ray))
        discriminant (- (* b b) (* 4 a c))]
    (if (< discriminant 0)
      []
      (let [t1 (/ (- (- b) (sqrt discriminant)) (* 2 a))
            t2 (/ (+ (- b) (sqrt discriminant)) (* 2 a))]
        [(intersection t1 sphere) (intersection t2 sphere)]))))
