(ns ray-tracer-challenge.logic.spheres
  (:require [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
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
