(ns ray-tracer-challenge.canvas.canvas-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [ray-tracer-challenge.canvas.canvas :refer :all]))

(defn- all-coords [canvas] (for [x (range (:width canvas)) y (range (:height canvas))] [x y]))
(defn- black-pixel-at? [canvas [x y]] (is (= [0 0 0] (pixel-at canvas x y))))
(defn- black-canvas? [canvas] (every? (partial black-pixel-at? canvas) (all-coords canvas)))

(deftest about-canvas

  (testing "should initially create black canvas"
    (let [canvas (canvas 10 20)]
      (is (= 10 (:width canvas)))
      (is (= 20 (:height canvas)))
      (black-canvas? canvas)))

  (testing "should be able to write pixels"
    (let [red [1 0 0]
          green [0 1 0]
          blue [0 0 1]
          canvas (-> (canvas 10 20)
                     (write-pixel 0 0 red)
                     (write-pixel 2 3 green)
                     (write-pixel 9 19 blue))]
      (is (= red (pixel-at canvas 0 0)))
      (is (= green (pixel-at canvas 2 3)))
      (is (= blue (pixel-at canvas 9 19)))))

  (testing "should ignore writing pixels outside bound"
    (let [canvas (canvas 10 20)
          test-pixel-at (fn [x y] (black-canvas? (write-pixel canvas x y [1 1 1])))]
      (test-pixel-at -1 0)
      (test-pixel-at 0 -1)
      (test-pixel-at (:width canvas) 0)
      (test-pixel-at 0 (:height canvas)))))

(deftest about-canvas-ppm

  (testing "should construct header"
    (let [ppm (canvas-to-ppm (canvas 5 3))]
      (is (= ["P3" "5 3" "255"]
             (take 3 (str/split-lines ppm))))))

  (testing "should construct pixel data"
    (let [ppm (-> (canvas 5 3)
                  (write-pixel 0 0 [1.5 0 0])
                  (write-pixel 2 1 [0 0.5 0])
                  (write-pixel 4 2 [-0.5 0 1])
                  (canvas-to-ppm))]
      (is (= ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]
             (drop 3 (str/split-lines ppm))))))

  (testing "should split long lines"
    (let [set-pixel (fn [canvas [x y]] (write-pixel canvas x y [1 0.8 0.6]))
          set-every-pixel (fn [canvas] (reduce set-pixel canvas (all-coords canvas)))
          ppm (-> (canvas 10 2)
                  (set-every-pixel)
                  (canvas-to-ppm))]
      (is (= ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
              "153 255 204 153 255 204 153 255 204 153 255 204 153"
              "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
              "153 255 204 153 255 204 153 255 204 153 255 204 153"]
             (drop 3 (str/split-lines ppm))))))

  (testing "should be terminated by a newline"
    (is (str/ends-with? (canvas-to-ppm (canvas 5 3)) "\n"))))
