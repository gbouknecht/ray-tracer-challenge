(ns ray-tracer-challenge.putting-it-together.chapter-16-dices
  (:require [ray-tracer-challenge.canvas.camera :refer :all]
            [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.csg :refer :all]
            [ray-tracer-challenge.logic.cubes :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.patterns :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.logic.world :refer :all]
            [ray-tracer-challenge.putting-it-together.progress-reporter :refer :all]))

(defn- pip [x z] (sphere :transform (multiply-matrices (translation x 0 z) (scaling 0.2 0.2 0.2))))
(defn- dice [color transform]
  (let [one (group :transform (translation 0 -1 0)
                   :children [(pip 0 0)])
        two (group :transform (multiply-matrices (translation 0 0 -1) (rotation-x (/ Math/PI 2)))
                   :children [(pip -0.5 0.5) (pip 0.5 -0.5)])
        three (group :transform (multiply-matrices (translation 1 0 0) (rotation-z (/ Math/PI 2)))
                     :children [(pip 0 0) (pip -0.5 0.5) (pip 0.5 -0.5)])
        four (group :transform (multiply-matrices (translation -1 0 0) (rotation-z (/ Math/PI 2)))
                    :children [(pip -0.5 0.5) (pip 0.5 -0.5) (pip -0.5 -0.5) (pip 0.5 0.5)])
        five (group :transform (multiply-matrices (translation 0 0 1) (rotation-x (/ Math/PI 2)))
                    :children [(pip 0 0) (pip -0.5 0.5) (pip 0.5 -0.5) (pip -0.5 -0.5) (pip 0.5 0.5)])
        six (group :transform (translation 0 1 0) :children [(pip -0.5 0.5) (pip 0.5 -0.5) (pip -0.5 -0.5) (pip 0.5 0.5) (pip 0 0.5) (pip 0 -0.5)])
        cube (cube :material (material :color color :reflective 0.2))
        csg (csg :difference cube (group :children [one two three four five six]))
        dice (group :transform transform :children [csg])]
    dice))
(defn -main []
  (let [objects {:room  (cube :transform (multiply-matrices (scaling 15 15 15) (translation 0 1 0))
                              :material (material :pattern (checkers-pattern white (color 0.7 0.7 0.7) :transform (scaling 0.25 0.25 0.25))
                                                  :specular 0))
                 :dice1 (dice red (translation 0 1 0))
                 :dice2 (dice green (multiply-matrices (translation -2 1 -2)
                                                       (rotation-y (/ Math/PI 3))))
                 :dice3 (dice blue (multiply-matrices (translation -1.5 3 -0.5)
                                                      (rotation-y (/ Math/PI 6))
                                                      (rotation-x Math/PI)
                                                      (rotation-z Math/PI)))
                 :lens  (csg :intersection
                             (glass-sphere :transform (translation -0.9 0 0))
                             (glass-sphere :transform (translation 0.9 0 0))
                             :transform (multiply-matrices (translation 3 2 -2) (rotation-y (/ Math/PI 6)) (scaling 1 3.5 3.5)))}
        world (world :light (point-light (point 0 10 -10) white)
                     :objects (vals objects))
        camera (camera 100 50 (/ Math/PI 3)
                       :transform (view-transform (point 10 6 -5) (point 0 2 0) (vektor 0 1 0)))
        canvas (render camera world (make-report-progress-fn))]
    (spit "target/dices.ppm" (canvas-to-ppm canvas))))
