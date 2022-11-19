(ns ray-tracer-challenge.logic.world
  (:require [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
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
(defn shade-hit [world comps]
  (lighting (get-in comps [:object :material])
            (:light world)
            (:over-point comps)
            (:eye-vektor comps)
            (:normal-vektor comps)
            (shadowed? world (:over-point comps))))
(defn color-at [world ray]
  (let [hit (hit (intersect-world world ray))]
    (if (nil? hit) black (shade-hit world (prepare-computation hit ray)))))
