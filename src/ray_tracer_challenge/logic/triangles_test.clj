(ns ray-tracer-challenge.logic.triangles-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
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
      (is (roughly (:normal triangle) (local-normal-at triangle (point 0 0.5 0) nil)))
      (is (roughly (:normal triangle) (local-normal-at triangle (point -0.5 0.75 0) nil)))
      (is (roughly (:normal triangle) (local-normal-at triangle (point 0.5 0.25 0) nil))))))

(deftest about-smooth-triangles

  (let [p1 (point 0 1 0)
        p2 (point -1 0 0)
        p3 (point 1 0 0)
        n1 (vektor 0 1 0)
        n2 (vektor -1 0 0)
        n3 (vektor 1 0 0)
        tri (smooth-triangle p1 p2 p3 n1 n2 n3)]

    (testing "should store the three vertex points, as well as the normal vectors for those points"
      (is (roughly p1 (:p1 tri)))
      (is (roughly p2 (:p2 tri)))
      (is (roughly p3 (:p3 tri)))
      (is (roughly n1 (:n1 tri)))
      (is (roughly n2 (:n2 tri)))
      (is (roughly n3 (:n3 tri))))

    (testing "should store u/v when intersected"
      (let [ray (ray (point -0.2 0.3 -2) (vektor 0 0 1))
            intersection (first (local-intersect tri ray))]
        (is (roughly 0.45 (:u intersection)))
        (is (roughly 0.25 (:v intersection)))))

    (testing "should use u/v to interpolate the normal"
      (let [intersection (intersection 1 tri 0.45 0.25)]
        (is (roughly (vektor -0.5547 0.83205 0) (normal-at tri (point 0 0 0) intersection {})))))

    (testing "should prepare normal"
      (let [intersection (intersection 1 tri 0.45 0.25)
            ray (ray (point -0.2 0.3 -2) (vektor 0 0 1))
            comps (prepare-computation intersection ray {})]
        (is (roughly (vektor -0.5547 0.83205 0) (:normal-vektor comps)))))))
