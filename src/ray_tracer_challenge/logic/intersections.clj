(ns ray-tracer-challenge.logic.intersections
  (:require [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn prepare-computation [intersection ray]
  (let [t (:t intersection)
        object (:object intersection)
        point (position ray t)
        eye-vektor (negate-tuple (:direction ray))
        normal-vektor (normal-at object point)
        inside (neg? (dot-product-tuples normal-vektor eye-vektor))]
    {:t             t
     :object        object
     :point         point
     :eye-vektor    eye-vektor
     :inside        inside
     :normal-vektor (if inside (negate-tuple normal-vektor) normal-vektor)}))
