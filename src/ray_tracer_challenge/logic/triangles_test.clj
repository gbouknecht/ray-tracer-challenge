(ns ray-tracer-challenge.logic.triangles-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.triangles :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-triangles

  (testing "should precompute two edge vectors and normal during construction"
    (let [p1 (point 0 1 0)
          p2 (point -1 0 0)
          p3 (point 1 0 0)
          triangle (triangle p1 p2 p3)]
      (is (roughly p1 (:p1 triangle)))
      (is (roughly p2 (:p2 triangle)))
      (is (roughly p3 (:p3 triangle)))
      (is (roughly (vektor -1 -1 0) (:e1 triangle)))
      (is (roughly (vektor 1 -1 0) (:e2 triangle)))
      (is (roughly (vektor 0 0 -1) (:normal triangle)))))

  (testing "should have no intersection with ray parallel to triangle"
    (let [triangle (triangle (point 0 1 0) (point -1 0 0) (point 1 0 0))
          ray (ray (point 0 -1 -2) (vektor 0 1 0))]
      (is (empty? (local-intersect triangle ray)))))

  (testing "should have no intersection with ray that misses p1-p3 edge"
    (let [triangle (triangle (point 0 1 0) (point -1 0 0) (point 1 0 0))
          ray (ray (point 1 1 -2) (vektor 0 0 1))]
      (is (empty? (local-intersect triangle ray)))))

  (testing "should have no intersection with ray that misses p1-p2 edge"
    (let [triangle (triangle (point 0 1 0) (point -1 0 0) (point 1 0 0))
          ray (ray (point -1 1 -2) (vektor 0 0 1))]
      (is (empty? (local-intersect triangle ray)))))

  (testing "should have no intersection with ray that misses p2-p3 edge"
    (let [triangle (triangle (point 0 1 0) (point -1 0 0) (point 1 0 0))
          ray (ray (point 0 -1 -2) (vektor 0 0 1))]
      (is (empty? (local-intersect triangle ray)))))

  (testing "should be able to calculate intersection"
    (let [triangle (triangle (point 0 1 0) (point -1 0 0) (point 1 0 0))
          ray (ray (point 0 0.5 -2) (vektor 0 0 1))]
      (is (roughly [(intersection 2 triangle)] (local-intersect triangle ray)))))

  (testing "should be able to calculate normal"
    (let [triangle (triangle (point 0 1 0) (point -1 0 0) (point 1 0 0))]
      (is (roughly (:normal triangle) (local-normal-at triangle (point 0 0.5 0))))
      (is (roughly (:normal triangle) (local-normal-at triangle (point -0.5 0.75 0))))
      (is (roughly (:normal triangle) (local-normal-at triangle (point 0.5 0.25 0)))))))
