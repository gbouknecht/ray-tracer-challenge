(ns ray-tracer-challenge.logic.transformations
  (:require [ray-tracer-challenge.logic.matrices :refer :all]))

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
