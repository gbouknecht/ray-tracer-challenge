(ns ray-tracer-challenge.putting-it-together.chapter-06-sphere-3d-rendering
  (:require [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn -main []
  (let [ray-origin (point 0 0 -5)
        wall-z 10
        wall-size 7.0
        canvas-pixels 100
        pixel-size (/ wall-size canvas-pixels)
        half-wall-size (/ wall-size 2)
        shape (sphere :material (material :color (color 1 0.2 1)))
        light (point-light (point -10 10 -10) (color 1 1 1))
        coords (for [x (range canvas-pixels) y (range canvas-pixels)] [x y])
        hit-color (fn [x y]
                    (let [wall-x (+ (- half-wall-size) (* pixel-size x))
                          wall-y (- half-wall-size (* pixel-size y))
                          wall-point (point wall-x wall-y wall-z)
                          ray (ray ray-origin (normalize-vektor (subtract-tuples wall-point ray-origin)))
                          hit (hit (intersect shape ray))]
                      (if (nil? hit)
                        black
                        (let [point (position ray (:t hit))
                              eye-vektor (negate-tuple (:direction ray))
                              normal-vektor (normal-at (:object hit) point nil {})]
                          (lighting (-> hit :object :material) shape light point eye-vektor normal-vektor false)))))
        write-pixel (fn [canvas [x y]] (write-pixel canvas x y (hit-color x y)))
        canvas (reduce write-pixel (canvas canvas-pixels canvas-pixels) coords)]
    (spit "target/sphere-3d-rendering.ppm" (canvas-to-ppm canvas))))
