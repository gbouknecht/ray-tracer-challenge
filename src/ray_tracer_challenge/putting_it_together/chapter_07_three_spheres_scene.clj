(ns ray-tracer-challenge.putting-it-together.chapter-07-three-spheres-scene
  (:require [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.camera :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.logic.world :refer :all]))

(defn -main []
  (let [floor (sphere :transform (scaling 10 0.01 10)
                      :material (material :color (color 1 0.9 0.9) :specular 0))
        left-wall (sphere :transform (reduce multiply-matrices [(translation 0 0 5)
                                                                (rotation-y (- (/ Math/PI 4)))
                                                                (rotation-x (/ Math/PI 2))
                                                                (scaling 10 0.01 10)])
                          :material (:material floor))
        right-wall (sphere :transform (reduce multiply-matrices [(translation 0 0 5)
                                                                 (rotation-y (/ Math/PI 4))
                                                                 (rotation-x (/ Math/PI 2))
                                                                 (scaling 10 0.01 10)])
                           :material (:material floor))
        middle (sphere :transform (translation -0.5 1 0.5)
                       :material (material :color (color 0.1 1 0.5) :diffuse 0.7 :specular 0.3))
        right (sphere :transform (multiply-matrices (translation 1.5 0.5 -0.5) (scaling 0.5 0.5 0.5))
                      :material (material :color (color 0.5 1 0.1) :diffuse 0.7 :specular 0.3))
        left (sphere :transform (multiply-matrices (translation -1.5 0.33 -0.75) (scaling 0.33 0.33 0.33))
                     :material (material :color (color 1 0.8 0.1) :diffuse 0.7 :specular 0.3))
        world (world :light (point-light (point -10 10 -10) (color 1 1 1))
                     :objects [floor left-wall right-wall middle right left])
        camera (camera 100 50 (/ Math/PI 3)
                       :transform (view-transform (point 0 1.5 -5) (point 0 1 0) (vektor 0 1 0)))
        canvas (render camera world)]
    (spit "target/three-spheres-scene.ppm" (canvas-to-ppm canvas))))
