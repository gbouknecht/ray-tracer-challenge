(ns ray-tracer-challenge.putting-it-together.chapter-11-glass-and-mirrors
  (:require [ray-tracer-challenge.canvas.camera :refer :all]
            [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.patterns :refer :all]
            [ray-tracer-challenge.logic.planes :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.logic.world :refer :all]
            [ray-tracer-challenge.putting-it-together.progress-reporter :refer :all]))

(defn -main []
  (let [world (world :light (point-light (point -2 4 -4) (color 1 1 1))
                     :objects [(plane :material (material :pattern (checkers-pattern white black :transform (scaling 0.25 0.25 0.25))))
                               (plane :transform (multiply-matrices (translation 0 0 10) (rotation-x (/ Math/PI 2)))
                                      :material (material :color (color 1 0.84 0) :diffuse 0.5 :reflective 1.0))
                               (plane :transform (multiply-matrices (rotation-y (/ Math/PI 4)) (rotation-x (/ Math/PI 2)))
                                      :material (material :reflective 0.9 :transparency 0.9 :refractive-index 1.5))
                               (sphere :transform (multiply-matrices (translation 0 1.5 0) (scaling 1.5 1.5 1.5))
                                       :material (material :color green))
                               (sphere :transform (multiply-matrices (translation 4 0.5 2) (scaling 0.5 0.5 0.5))
                                       :material (material :color red))
                               (sphere :transform (translation -3.5 1 -2)
                                       :material (material :color blue))])
        camera (camera 100 50 (/ Math/PI 2)
                       :transform (view-transform (point 1 0.5 -7) (point 0 1 0) (vektor 0 1 0)))
        canvas (render camera world report-progress)]
    (spit "target/glass-and-mirrors.ppm" (canvas-to-ppm canvas))))
