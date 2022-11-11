(ns ray-tracer-challenge.logic.transformations
  (:require [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn translation [x y z]
  (matrix [[1 0 0 x]
           [0 1 0 y]
           [0 0 1 z]
           [0 0 0 1]]))
(defn scaling [x y z]
  (matrix [[x 0 0 0]
           [0 y 0 0]
           [0 0 z 0]
           [0 0 0 1]]))
(defn rotation-x [radians]
  (matrix [[1 0 0 0]
           [0 (Math/cos radians) (- (Math/sin radians)) 0]
           [0 (Math/sin radians) (Math/cos radians) 0]
           [0 0 0 1]]))
(defn rotation-y [radians]
  (matrix [[(Math/cos radians) 0 (Math/sin radians) 0]
           [0 1 0 0]
           [(- (Math/sin radians)) 0 (Math/cos radians) 0]
           [0 0 0 1]]))
(defn rotation-z [radians]
  (matrix [[(Math/cos radians) (- (Math/sin radians)) 0 0]
           [(Math/sin radians) (Math/cos radians) 0 0]
           [0 0 1 0]
           [0 0 0 1]]))
(defn shearing [xy xz yx yz zx zy]
  (matrix [[1 xy xz 0]
           [yx 1 yz 0]
           [zx zy 1 0]
           [0 0 0 1]]))
(defn view-transform [[from-x from-y from-z :as from] to up]
  (let [[forward-x forward-y forward-z :as forward] (normalize-vektor (subtract-tuples to from))
        [left-x left-y left-z :as left] (cross-product-vektors forward (normalize-vektor up))
        [true-up-x true-up-y true-up-z] (cross-product-vektors left forward)
        orientation (matrix [[left-x left-y left-z 0]
                             [true-up-x true-up-y true-up-z 0]
                             [(- forward-x) (- forward-y) (- forward-z) 0]
                             [0 0 0 1]])]
    (multiply-matrices orientation (translation (- from-x) (- from-y) (- from-z)))))
