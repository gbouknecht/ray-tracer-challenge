(ns ray-tracer-challenge.logic.shapes
  (:require [ray-tracer-challenge.logic.materials :as materials]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn shape [local-intersect local-normal-at transform material]
  {:uuid            (random-uuid)
   :transform       (or transform identity-matrix)
   :material        (or material (materials/material))
   :local-intersect local-intersect
   :local-normal-at local-normal-at})
(defn intersect [shape ray]
  (let [local-ray (transform ray (inverse (:transform shape)))]
    ((:local-intersect shape) shape local-ray)))
(defn normal-at [shape world-point]
  (let [local-point (multiply-matrix-by-tuple (inverse (:transform shape)) world-point)
        local-normal ((:local-normal-at shape) shape local-point)
        [wnx wny wnz] (multiply-matrix-by-tuple (transpose (inverse (:transform shape))) local-normal)
        world-normal (vektor wnx wny wnz)]
    (normalize-vektor world-normal)))