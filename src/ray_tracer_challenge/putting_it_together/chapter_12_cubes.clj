(ns ray-tracer-challenge.putting-it-together.chapter-12-cubes
  (:require [ray-tracer-challenge.canvas.camera :refer :all]
            [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.constants :refer :all]
            [ray-tracer-challenge.logic.cubes :refer :all]
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
  (let [objects {:room         (cube :transform (scaling 10 5 15)
                                     :material (material :color (color 0.71 0.79 0.71) :specular 0))
                 :floor        (plane :transform (translation 0 (+ -5 (/ epsilon 2)) 0)
                                      :material (material :pattern (checkers-pattern white (color 0.5 0.5 0.5)) :reflective 0.1))
                 :table-top    (cube :transform (multiply-matrices (translation 0 -2.9 5)
                                                                   (scaling 2 0.1 1))
                                     :material (material :color black))
                 :table-leg-1  (cube :transform (multiply-matrices (translation -1.9 -4 4.1)
                                                                   (scaling 0.1 1 0.1))
                                     :material (material :color black))
                 :table-leg-2  (cube :transform (multiply-matrices (translation -1.9 -4 5.9)
                                                                   (scaling 0.1 1 0.1))
                                     :material (material :color black))
                 :table-leg-3  (cube :transform (multiply-matrices (translation 1.9 -4 5.9)
                                                                   (scaling 0.1 1 0.1))
                                     :material (material :color black))
                 :table-leg-4  (cube :transform (multiply-matrices (translation 1.9 -4 4.1)
                                                                   (scaling 0.1 1 0.1))
                                     :material (material :color black))
                 :ball-red     (sphere :transform (multiply-matrices (translation -1.25 -2.3 5)
                                                                     (scaling 0.5 0.5 0.5))
                                       :material (material :color red))
                 :ball-green   (sphere :transform (multiply-matrices (translation 0 -2.3 5)
                                                                     (scaling 0.5 0.5 0.5))
                                       :material (material :color green))
                 :ball-blue    (sphere :transform (multiply-matrices (translation 1.25 -2.3 5)
                                                                     (scaling 0.5 0.5 0.5))
                                       :material (material :color blue))
                 :cube         (cube :transform (multiply-matrices (translation 4.5 -3.59 10)
                                                                   (rotation-y (/ Math/PI 4))
                                                                   (rotation-x (/ Math/PI 4)))
                                     :material (material :specular 1.0 :shininess 300 :reflective 0.9 :transparency 1.0 :refractive-index 1.52))
                 :ball-in-cube (sphere :transform (multiply-matrices (translation 4.5 -3.59 10)
                                                                     (scaling 0.5 0.5 0.5))
                                       :material (material :color red))
                 :mirror       (cube :transform (multiply-matrices (translation 0 -2 (- 15 (/ epsilon 2)))
                                                                   (scaling 8 2 0.1))
                                     :material (material :ambient 0.0 :diffuse 0.0 :reflective 1.0))}
        world (world :light (point-light (point -5 4 -14) (color 1 1 1))
                     :objects (vals objects))
        camera (camera 100 50 (/ Math/PI 2)
                       :transform (view-transform (point 3 3 -7) (point 0 0 0) (vektor 0 1 0)))
        canvas (render camera world report-progress)]
    (spit "target/cubes.ppm" (canvas-to-ppm canvas))
    (shutdown-agents)))
