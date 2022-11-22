(ns ray-tracer-challenge.logic.spheres
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn sphere [& {:keys [transform material]}]
  (letfn [(local-intersect [sphere local-ray]
            (let [sphere-to-ray (subtract-tuples (:origin local-ray) (point 0 0 0))
                  a (dot-product-tuples (:direction local-ray) (:direction local-ray))
                  b (* (dot-product-tuples (:direction local-ray) sphere-to-ray) 2)
                  c (dec (dot-product-tuples sphere-to-ray sphere-to-ray))
                  discriminant (- (* b b) (* 4 a c))]
              (if (< discriminant 0)
                []
                (let [t1 (/ (- (- b) (sqrt discriminant)) (* 2 a))
                      t2 (/ (+ (- b) (sqrt discriminant)) (* 2 a))]
                  [(intersection t1 sphere) (intersection t2 sphere)]))))
          (local-normal-at [_ local-point] (subtract-tuples local-point (point 0 0 0)))]
    (shape local-intersect local-normal-at transform material)))
