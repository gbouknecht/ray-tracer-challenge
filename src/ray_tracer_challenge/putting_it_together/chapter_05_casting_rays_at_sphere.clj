(ns ray-tracer-challenge.putting-it-together.chapter-05-casting-rays-at-sphere
  (:require [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
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
        shape (sphere :transform (multiply-matrices (shearing 1 0 0 0 0 0) (scaling 0.5 1 1)))
        coords (for [x (range canvas-pixels) y (range canvas-pixels)] [x y])
        hit? (fn [x y]
               (let [wall-x (+ (- half-wall-size) (* pixel-size x))
                     wall-y (- half-wall-size (* pixel-size y))
                     wall-point (point wall-x wall-y wall-z)
                     ray (ray ray-origin (normalize-vektor (subtract-tuples wall-point ray-origin)))]
                 (some? (hit (intersect shape ray)))))
        write-pixel (fn [canvas [x y]] (write-pixel canvas x y (if (hit? x y) red black)))
        canvas (reduce write-pixel (canvas canvas-pixels canvas-pixels) coords)]
    (spit "target/casting-rays-at-sphere.ppm" (canvas-to-ppm canvas))))
