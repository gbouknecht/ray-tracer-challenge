(ns ray-tracer-challenge.logic.world
  (:require [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn world [& {:keys [light objects]
                :or   {light nil objects []}}]
  {:light   light
   :objects objects})
(defn intersect-world [world ray] (->> (:objects world) (mapcat #(intersect % ray)) (sort-by :t)))
(defn shadowed? [world point]
  (let [vektor-to-light-source (subtract-tuples (get-in world [:light :position]) point)
        distance (magnitude-vektor vektor-to-light-source)
        ray (ray point (normalize-vektor vektor-to-light-source))
        hit (hit (intersect-world world ray))]
    (and hit (< (:t hit) distance))))
(declare reflected-color)
(defn shade-hit
  ([world comps] (shade-hit world comps 5))
  ([world comps remaining]
   (let [surface-color (lighting (get-in comps [:object :material])
                                 (:object comps)
                                 (:light world)
                                 (:over-point comps)
                                 (:eye-vektor comps)
                                 (:normal-vektor comps)
                                 (shadowed? world (:over-point comps)))
         reflected-color (reflected-color world comps remaining)]
     (add-colors surface-color reflected-color))))
(defn color-at
  ([world ray] (color-at world ray 5))
  ([world ray remaining]
   (let [hit (hit (intersect-world world ray))]
     (if (nil? hit) black (shade-hit world (prepare-computation hit ray) remaining)))))
(defn reflected-color
  ([world comps] (reflected-color world comps 5))
  ([world comps remaining]
   (let [reflective (get-in comps [:object :material :reflective])]
     (if (or (< remaining 1) (= reflective 0.0))
       black
       (let [reflect-ray (ray (:over-point comps) (:reflect-vektor comps))
             color (color-at world reflect-ray (dec remaining))]
         (multiply-color color reflective))))))
