(ns ray-tracer-challenge.logic.intersections-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.constants :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.planes :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-intersections

  (testing "should be able to precompute state of an intersection"
    (let [ray (ray (point 0 0 -5) (vektor 0 0 1))
          shape (sphere)
          intersection (intersection 4 shape)
          comps (prepare-computation intersection ray)]
      (is (= (:t intersection) (:t comps)))
      (is (= (:object intersection) (:object comps)))
      (is (roughly (point 0 0 -1) (:point comps)))
      (is (roughly (vektor 0 0 -1) (:eye-vektor comps)))
      (is (roughly (vektor 0 0 -1) (:normal-vektor comps)))))

  (testing "should be able to precompute state of an intersection on the outside"
    (let [ray (ray (point 0 0 -5) (vektor 0 0 1))
          shape (sphere)
          intersection (intersection 4 shape)
          comps (prepare-computation intersection ray)]
      (is (false? (:inside comps)))))

  (testing "should be able to precompute state of an intersection on the inside"
    (let [ray (ray (point 0 0 0) (vektor 0 0 1))
          shape (sphere)
          intersection (intersection 1 shape)
          comps (prepare-computation intersection ray)]
      (is (roughly (point 0 0 1) (:point comps)))
      (is (roughly (vektor 0 0 -1) (:eye-vektor comps)))
      (is (true? (:inside comps)))
      (is (roughly (vektor 0 0 -1) (:normal-vektor comps)))))

  (testing "should offset the point to take imprecise representation of floating point numbers into account"
    (let [ray (ray (point 0 0 -5) (vektor 0 0 1))
          shape (sphere :transform (translation 0 0 1))
          intersection (intersection 5 shape)
          comps (prepare-computation intersection ray)
          [_ _ point-z] (:point comps)
          [_ _ over-point-z] (:over-point comps)]
      (is (< over-point-z (- (/ epsilon 2))))
      (is (> point-z over-point-z))))

  (testing "should be able to precompute reflection vector"
    (let [ray (ray (point 0 1 -1) (vektor 0 (- (/ (sqrt 2) 2)) (/ (sqrt 2) 2)))
          shape (plane)
          intersection (intersection (sqrt 2) shape)
          comps (prepare-computation intersection ray)]
      (is (roughly (vektor 0 (/ (sqrt 2) 2) (/ (sqrt 2) 2)) (:reflect-vektor comps))))))
