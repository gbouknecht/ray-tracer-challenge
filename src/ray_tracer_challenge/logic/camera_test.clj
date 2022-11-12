(ns ray-tracer-challenge.logic.camera-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.camera :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.logic.world-test :refer [default-world]]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-camera

  (testing "should be able to construct a camera"
    (let [hsize 160
          vsize 120
          field-of-view (/ Math/PI 2)
          camera (camera hsize vsize field-of-view)]
      (is (= hsize (:hsize camera)))
      (is (= vsize (:vsize camera)))
      (is (= field-of-view (:field-of-view camera)))
      (is (roughly identity-matrix (:transform camera)))))

  (testing "should correctly calculate pixel size for a horizontal canvas"
    (is (roughly 0.01 (:pixel-size (camera 200 125 (/ Math/PI 2))))))

  (testing "should correctly calculate pixel size for a vertical canvas"
    (is (roughly 0.01 (:pixel-size (camera 125 200 (/ Math/PI 2)))))))

(deftest about-casting-rays-from-camera

  (testing "should be able to construct ray through center of canvas"
    (let [camera (camera 201 101 (/ Math/PI 2))
          ray (ray-for-pixel camera 100 50)]
      (is (roughly (point 0 0 0) (:origin ray)))
      (is (roughly (vektor 0 0 -1) (:direction ray)))))

  (testing "should be able to construct ray through corner of canvas"
    (let [camera (camera 201 101 (/ Math/PI 2))
          ray (ray-for-pixel camera 0 0)]
      (is (roughly (point 0 0 0) (:origin ray)))
      (is (roughly (vektor 0.66519 0.33259 -0.66851) (:direction ray)))))

  (testing "should be able to construct ray when camera is transformed"
    (let [camera (camera 201 101 (/ Math/PI 2)
                         :transform (multiply-matrices (rotation-y (/ Math/PI 4)) (translation 0 -2 5)))
          ray (ray-for-pixel camera 100 50)]
      (is (roughly (point 0 2 -5) (:origin ray)))
      (is (roughly (vektor (/ (sqrt 2) 2) 0 (- (/ (sqrt 2) 2))) (:direction ray))))))

(deftest about-rendering-world-with-camera

  (testing "should render middle of canvas with expected color"
    (let [world (default-world)
          from (point 0 0 -5)
          to (point 0 0 0)
          up (vektor 0 1 0)
          camera (camera 11 11 (/ Math/PI 2) :transform (view-transform from to up))
          image (render camera world)]
      (is (roughly (color 0.38066 0.47583 0.2855) (pixel-at image 5 5))))))