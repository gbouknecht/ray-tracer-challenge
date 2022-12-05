(ns ray-tracer-challenge.logic.intersections
  (:require [ray-tracer-challenge.logic.constants :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn prepare-computation [intersection ray]
  (let [t (:t intersection)
        object (:object intersection)
        point (position ray t)
        eye-vektor (negate-tuple (:direction ray))
        [normal-vektor inside] (let [normal-vektor (normal-at object point)
                                     inside (neg? (dot-product-tuples normal-vektor eye-vektor))]
                                 [(if inside (negate-tuple normal-vektor) normal-vektor) inside])]
    {:t              t
     :object         object
     :point          point
     :eye-vektor     eye-vektor
     :inside         inside
     :normal-vektor  normal-vektor
     :over-point     (add-tuples point (multiply-tuple normal-vektor epsilon))
     :reflect-vektor (reflect (:direction ray) normal-vektor)}))
