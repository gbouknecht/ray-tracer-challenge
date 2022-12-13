(ns ray-tracer-challenge.putting-it-together.chapter-09-hexagonal-room-scene
  (:require [ray-tracer-challenge.canvas.camera :refer :all]
            [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.planes :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.logic.world :refer :all]
            [ray-tracer-challenge.putting-it-together.progress-reporter :refer :all]))

(defn -main []
  (let [wall (fn [factor]
               (plane :transform (multiply-matrices (rotation-y (* factor (/ Math/PI 3)))
                                                    (translation 0 0 8)
                                                    (rotation-x (/ Math/PI 2)))
                      :material (material :color white :specular 0)))
        world (world :light (point-light (point -2 4 -4) (color 1 1 1))
                     :objects [(plane :material (material :color (color 0.85 0.72 0.53) :specular 0))
                               (wall 1)
                               (wall 0)
                               (wall -1)
                               (wall -2)
                               (sphere :transform (multiply-matrices (translation 0 1.5 0) (scaling 1.5 1.5 1.5))
                                       :material (material :color (color 0.3 1 0)))
                               (sphere :transform (multiply-matrices (translation -2 0.5 -2) (scaling 0.5 0.5 0.5))
                                       :material (material :color (color 1 0.2 0)))
                               (sphere :transform (multiply-matrices (translation 3 0 -1.2) (scaling 0.8 0.8 0.8))
                                       :material (material :color (color 0 1 1)))])
        camera (camera 100 50 (/ Math/PI 2)
                       :transform (view-transform (point 1.9 2 -7) (point 0 1 0) (vektor 0 1 0)))
        canvas (render camera world report-progress)]
    (spit "target/hexagonal-room-scene.ppm" (canvas-to-ppm canvas))))
