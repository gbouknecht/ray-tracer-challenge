(ns ray-tracer-challenge.canvas.canvas-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [ray-tracer-challenge.canvas.canvas :refer :all]))

(defn- all-coords [canvas] (for [x (range (:width canvas)) y (range (:height canvas))] [x y]))

(deftest about-canvas

  (testing
    "should initially create black canvas"
    (let [canvas (canvas 10 20)]
      (is (= (:width canvas) 10))
      (is (= (:height canvas) 20))
      (letfn [(black-pixel-at? [[x y]] (is (= (pixel-at canvas x y) [0 0 0])))]
        (every? black-pixel-at? (all-coords canvas)))))

  (testing
    "should be able to write pixel"
    (let [canvas1 (canvas 10 20)
          red [1 0 1]
          canvas2 (write-pixel canvas1 2 3 red)]
      (is (= (pixel-at canvas2 2 3) red)))))

(deftest about-canvas-ppm

  (testing
    "should construct header"
    (let [ppm (canvas-to-ppm (canvas 5 3))]
      (is (= (take 3 (str/split-lines ppm))
             ["P3"
              "5 3"
              "255"]))))

  (testing
    "should construct pixel data"
    (let [ppm (-> (canvas 5 3)
                  (write-pixel 0 0 [1.5 0 0])
                  (write-pixel 2 1 [0 0.5 0])
                  (write-pixel 4 2 [-0.5 0 1])
                  (canvas-to-ppm))]
      (is (= (drop 3 (str/split-lines ppm))
             ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]))))

  (testing
    "should split long lines"
    (let [set-pixel (fn [canvas [x y]] (write-pixel canvas x y [1 0.8 0.6]))
          set-every-pixel (fn [canvas] (reduce set-pixel canvas (all-coords canvas)))
          ppm (-> (canvas 10 2)
                  (set-every-pixel)
                  (canvas-to-ppm))]
      (is (= (drop 3 (str/split-lines ppm))
             ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
              "153 255 204 153 255 204 153 255 204 153 255 204 153"
              "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
              "153 255 204 153 255 204 153 255 204 153 255 204 153"]))))

  (testing
    "should be terminated by a newline"
    (is (str/ends-with? (canvas-to-ppm (canvas 5 3)) "\n"))))
