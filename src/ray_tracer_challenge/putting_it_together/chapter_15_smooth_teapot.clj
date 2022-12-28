(ns ray-tracer-challenge.putting-it-together.chapter-15-smooth-teapot
  (:require [ray-tracer-challenge.canvas.camera :refer :all]
            [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.logic.world :refer :all]
            [ray-tracer-challenge.putting-it-together.progress-reporter :refer :all]
            [ray-tracer-challenge.wavefront.obj-file :refer :all]))

(defn -main []
  (let [world (world :light (point-light (point -5 15 -20) (color 1 1 1))
                     :objects [(obj-to-group (parse-obj-file (slurp "files/teapot-low.obj"))
                                             :transform (multiply-matrices (translation 0 -5 0) (rotation-x (- (/ Math/PI 2)))))])
        camera (camera 100 50 (/ Math/PI 2)
                       :transform (view-transform (point 10 10 -40) (point 0 0 0) (vektor 0 1 0)))
        canvas (render camera world (make-report-progress-fn))]
    (spit "target/smooth-teapot.ppm" (canvas-to-ppm canvas))))
