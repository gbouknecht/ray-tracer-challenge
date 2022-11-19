(ns ray-tracer-challenge.putting-it-together.chapter-08-shadows-scene
  (:require [ray-tracer-challenge.canvas.camera :refer :all]
            [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.logic.world :refer :all]))

(defn -main []
  (let [wall (sphere :transform (reduce multiply-matrices [(translation 0 0 5)
                                                           (scaling 10 10 1)])
                     :material (material :color (color 0.8 0.7 0.7) :diffuse 0.3 :specular 0))
        s1 (sphere :transform (multiply-matrices (translation -1.5 0 0) (scaling 0.30 0.4 0.4))
                   :material (material :color (color 0 1 1) :diffuse 0.7 :specular 0.3))
        s2 (sphere :transform (multiply-matrices (translation -0.85 0.45 0.5) (scaling 0.45 0.4 0.4))
                   :material (material :color (color 0 0 1) :diffuse 0.7 :specular 0.3))
        s3 (sphere :transform (reduce multiply-matrices [(translation -0.7 0.3 0.25)
                                                         (rotation-y (/ Math/PI 3))
                                                         (rotation-z (- (/ Math/PI 6)))
                                                         (scaling 0.45 0.1 0.4)])
                   :material (material :color (color 0 0 1) :diffuse 0.7 :specular 0.3))
        s4 (sphere :transform (reduce multiply-matrices [(translation -1 0.7 0.5)
                                                         (scaling 0.05 0.4 0.25)])
                   :material (material :color (color 0 0 1) :diffuse 0.7 :specular 0.3))
        s5 (sphere :transform (reduce multiply-matrices [(translation -0.9 0.7 0.1)
                                                         (rotation-y (/ Math/PI 3))
                                                         (scaling 0.4 0.09 0.25)])
                   :material (material :color (color 1 1 0) :diffuse 0.7 :specular 0.3))
        s6 (sphere :transform (reduce multiply-matrices [(translation -0.6 0.5 0.1)
                                                         (rotation-y (/ Math/PI 3))
                                                         (scaling 0.4 0.09 0.25)])
                   :material (material :color (color 0 1 0) :diffuse 0.7 :specular 0.3))
        s7 (sphere :transform (reduce multiply-matrices [(translation -0.55 0.4 0.1)
                                                         (rotation-y (/ Math/PI 3))
                                                         (scaling 0.4 0.09 0.25)])
                   :material (material :color (color 0 1 0) :diffuse 0.7 :specular 0.3))
        world (world :light (point-light (point -7 0 -8) (color 1 1 1))
                     :objects [wall s1 s2 s3 s4 s5 s6 s7])
        camera (camera 100 50 (/ Math/PI 3)
                       :transform (view-transform (point 0 0.5 -5) (point 0 0.5 0) (vektor 0 1 0)))
        canvas (render camera world)]
    (spit "target/shadows-scene.ppm" (canvas-to-ppm canvas))))
