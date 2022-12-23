(ns ray-tracer-challenge.putting-it-together.chapter-13-cylinders
  (:require [ray-tracer-challenge.canvas.camera :refer :all]
            [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.common :refer :all]
            [ray-tracer-challenge.logic.cubes :refer :all]
            [ray-tracer-challenge.logic.cylinders-and-cones :refer :all]
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
  (let [objects {:room      (cube :transform (scaling 12 5 15)
                                  :material (material :color (color 0.71 0.79 0.71) :specular 0))
                 :floor     (plane :transform (translation 0 (+ -5 (/ epsilon 2)) 0)
                                   :material (material :pattern (checkers-pattern white black) :reflective 0.1))
                 :trunk     (cylinder :material (material :color red)
                                      :minimum -2 :maximum 1 :closed true)
                 :left-leg  (cylinder :transform (multiply-matrices (translation -0.6 0 0) (scaling 0.4 1 0.4))
                                      :material (material :color green)
                                      :minimum -5 :maximum -2)
                 :right-leg (cylinder :transform (multiply-matrices (translation 0.6 0 0) (scaling 0.4 1 0.4))
                                      :material (material :color green)
                                      :minimum -5 :maximum -2)
                 :left-arm  (cylinder :transform (multiply-matrices (translation -1.2 0 0) (scaling 0.2 1 0.2))
                                      :material (material :color green)
                                      :minimum -1 :maximum 1 :closed true)
                 :right-arm (cylinder :transform (multiply-matrices (translation 1.2 0 0) (scaling 0.2 1 0.2))
                                      :material (material :color green)
                                      :minimum -1 :maximum 1 :closed true)
                 :head      (sphere :transform (multiply-matrices (translation 0 1.9 0) (scaling 0.8 0.9 0.8))
                                    :material (material :color (color 0.976 0.827 0.639)))
                 :hat       (cone :transform (multiply-matrices (translation 0 2.88 0)
                                                                (rotation-y (/ Math/PI -9))
                                                                (rotation-x (/ Math/PI 9))
                                                                (scaling 3 1 3))
                                  :material (material :color blue)
                                  :minimum -0.5 :maximum 0)
                 :mirror    (cylinder :transform (multiply-matrices (translation -9 2 0)
                                                                    (rotation-y (/ Math/PI 12))
                                                                    (rotation-z (/ Math/PI 2))
                                                                    (scaling 2 1 2))
                                      :material (material :ambient 0.0 :diffuse 0.0 :reflective 1.0)
                                      :minimum 0 :maximum 0.1 :closed true)}
        world (world :light (point-light (point -5 4 -14) (color 1 1 1))
                     :objects (vals objects))
        camera (camera 100 50 (/ Math/PI 2)
                       :transform (view-transform (point 3 3 -7) (point 0 0 0) (vektor 0 1 0)))
        canvas (render camera world (make-report-progress-fn))]
    (spit "target/cylinders-and-cones.ppm" (canvas-to-ppm canvas))))