(ns ray-tracer-challenge.putting-it-together.chapter-02-plotting-projectiles
  (:require [clojure.math.numeric-tower :refer [round]]
            [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.putting-it-together.chapter-01-firing-projectiles :refer [fire]]))

(defn write-square [canvas [x y]]
  (let [cx (round x)
        cy (- (:height canvas) (round y))
        write-pixel-with-delta (fn [canvas [dx dy]] (write-pixel canvas (+ cx dx) (+ cy dy) [1.0 0.47 0.47]))
        deltas (for [dx (range -2 3) dy (range -2 3)] [dx dy])]
    (reduce write-pixel-with-delta canvas deltas)))

(defn -main []
  (let [environment {:gravity (vektor 0 -0.1 0) :wind (vektor -0.01 0 0)}
        projectile {:position (point 0 1 0) :velocity (-> (vektor 1 1.8 0) normalize-vektor (multiply-tuple 11.25))}
        canvas (reduce write-square (canvas 900 550) (fire environment projectile))]
    (spit "target/plotted-projectile.ppm" (canvas-to-ppm canvas))))
