(ns ray-tracer-challenge.logic.world
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [ray-tracer-challenge.logic.colors :refer :all]
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
  (let [vektor-to-light-source (subtract-tuples (-> world :light :position) point)
        distance (magnitude-vektor vektor-to-light-source)
        ray (ray point (normalize-vektor vektor-to-light-source))
        hit (hit (intersect-world world ray))]
    (and hit (< (:t hit) distance))))
(declare reflected-color refracted-color)
(defn shade-hit
  ([world comps] (shade-hit world comps 5))
  ([world comps remaining]
   (let [material (-> comps :object :material)
         surface-color (lighting material
                                 (:object comps)
                                 (:light world)
                                 (:over-point comps)
                                 (:eye-vektor comps)
                                 (:normal-vektor comps)
                                 (shadowed? world (:over-point comps)))
         reflected-color (reflected-color world comps remaining)
         refracted-color (refracted-color world comps remaining)]
     (if (and (pos? (:reflective material)) (pos? (:transparency material)))
       (let [reflectance (schlick comps)]
         (add-colors surface-color
                     (multiply-color reflected-color reflectance)
                     (multiply-color refracted-color (- 1 reflectance))))
       (add-colors surface-color
                   reflected-color
                   refracted-color)))))
(defn color-at
  ([world ray shape-to-parent] (color-at world ray 5 shape-to-parent))
  ([world ray remaining shape-to-parent]
   (let [intersections (intersect-world world ray)
         hit (hit intersections)]
     (if (nil? hit) black (shade-hit world (prepare-computation hit ray intersections shape-to-parent) remaining)))))
(defn reflected-color
  ([world comps] (reflected-color world comps 5))
  ([world comps remaining]
   (let [reflective (-> comps :object :material :reflective)]
     (if (or (< remaining 1) (= reflective 0.0))
       black
       (let [reflect-ray (ray (:over-point comps) (:reflect-vektor comps))
             color (color-at world reflect-ray (dec remaining) (:shape-to-parent comps))]
         (multiply-color color reflective))))))
(defn refracted-color [world comps remaining]
  (let [transparency (-> comps :object :material :transparency)
        n-ratio (/ (:n1 comps) (:n2 comps))
        cos-i (dot-product-tuples (:eye-vektor comps) (:normal-vektor comps))
        sin2-t (* (* n-ratio n-ratio) (- 1 (* cos-i cos-i)))]
    (if (or (< remaining 1) (= transparency 0.0) (> sin2-t 1))
      black
      (let [cos-t (sqrt (- 1.0 sin2-t))
            direction (subtract-tuples (multiply-tuple (:normal-vektor comps) (- (* n-ratio cos-i) cos-t))
                                       (multiply-tuple (:eye-vektor comps) n-ratio))
            refract-ray (ray (:under-point comps) direction)]
        (multiply-color (color-at world refract-ray (dec remaining) (:shape-to-parent comps)) transparency)))))
