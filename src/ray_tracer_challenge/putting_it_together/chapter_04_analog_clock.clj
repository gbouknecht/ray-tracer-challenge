(ns ray-tracer-challenge.putting-it-together.chapter-04-analog-clock
  (:require [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.putting-it-together.chapter-02-plotting-projectiles :as ch02]))

(defn -main []
  (let [width 900
        height 900
        rotate #(multiply-matrix-by-tuple (rotation-z (- (/ Math/PI 6))) %)
        move #(multiply-matrix-by-tuple (translation (quot width 2) (quot height 2) 0) %)
        points (take 12 (iterate rotate (point 0 (quot (min width height) 3) 0)))
        write-square #(ch02/write-square %1 %2 [1.0 1.0 1.0] 11)
        canvas (->> points (map move) (reduce write-square (canvas width height)))]
    (spit "target/analog-clock.ppm" (canvas-to-ppm canvas))))
