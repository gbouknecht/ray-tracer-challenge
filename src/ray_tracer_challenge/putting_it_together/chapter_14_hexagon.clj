(ns ray-tracer-challenge.putting-it-together.chapter-14-hexagon
  (:require [ray-tracer-challenge.canvas.camera :refer :all]
            [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.cylinders-and-cones :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.logic.world :refer :all]
            [ray-tracer-challenge.putting-it-together.progress-reporter :refer :all]))

(defn- hexagon-corner [] (sphere :transform (multiply-matrices (translation 0 0 -1) (scaling 0.25 0.25 0.25))))
(defn- hexagon-edge []
  (cylinder :transform (multiply-matrices (translation 0 0 -1)
                                          (rotation-y (- (/ Math/PI 6)))
                                          (rotation-z (- (/ Math/PI 2)))
                                          (scaling 0.25 1 0.25))
            :minimum 0 :maximum 1))
(defn- hexagon-side [transform] (group :transform transform :children [(hexagon-corner) (hexagon-edge)]))
(defn- hexagon [] (group :transform (scaling 3 3 3) :children (for [n (range 6)] (hexagon-side (rotation-y (* n (/ Math/PI 3)))))))
(defn -main []
  (let [world (world :light (point-light (point -5 4 -14) (color 1 1 1))
                     :objects [(hexagon)])
        camera (camera 100 50 (/ Math/PI 2)
                       :transform (view-transform (point 3 4 -7) (point 0 0 0) (vektor 0 1 0)))
        canvas (render camera world (make-report-progress-fn))]
    (spit "target/hexagon.ppm" (canvas-to-ppm canvas))))
