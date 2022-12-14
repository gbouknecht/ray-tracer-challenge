(ns ray-tracer-challenge.putting-it-together.chapter-10-patterns
  (:require [ray-tracer-challenge.canvas.camera :refer :all]
            [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.patterns :refer :all]
            [ray-tracer-challenge.logic.planes :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.logic.world :refer :all]
            [ray-tracer-challenge.putting-it-together.progress-reporter :refer :all]))

(defn- blended-pattern [& patterns]
  (letfn [(blended-pattern-at [pattern point]
            (let [colors (map #(pattern-at % point) (:patterns pattern))]
              (multiply-color (reduce add-colors colors) (/ 1 (count colors)))))]
    (assoc (pattern nil blended-pattern-at) :patterns patterns)))

(defn -main []
  (let [world (world :light (point-light (point -2 4 -4) (color 1 1 1))
                     :objects [(plane :material (material :pattern (blended-pattern (checkers-pattern red green)
                                                                                    (ring-pattern green blue)
                                                                                    (gradient-pattern white black))))])
        camera (camera 100 50 (/ Math/PI 2)
                       :transform (view-transform (point 1.9 3 -7) (point 0 1 0) (vektor 0 1 0)))
        canvas (render camera world (make-report-progress-fn))]
    (spit "target/patterns.ppm" (canvas-to-ppm canvas))))
