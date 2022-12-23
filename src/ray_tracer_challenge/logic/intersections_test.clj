(ns ray-tracer-challenge.logic.intersections-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.common :refer :all]
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
          comps (prepare-computation intersection ray {})]
      (is (= (:t intersection) (:t comps)))
      (is (= (:object intersection) (:object comps)))
      (is (roughly (point 0 0 -1) (:point comps)))
      (is (roughly (vektor 0 0 -1) (:eye-vektor comps)))
      (is (roughly (vektor 0 0 -1) (:normal-vektor comps)))))

  (testing "should be able to precompute state of an intersection on the outside"
    (let [ray (ray (point 0 0 -5) (vektor 0 0 1))
          shape (sphere)
          intersection (intersection 4 shape)
          comps (prepare-computation intersection ray {})]
      (is (false? (:inside comps)))))

  (testing "should be able to precompute state of an intersection on the inside"
    (let [ray (ray (point 0 0 0) (vektor 0 0 1))
          shape (sphere)
          intersection (intersection 1 shape)
          comps (prepare-computation intersection ray {})]
      (is (roughly (point 0 0 1) (:point comps)))
      (is (roughly (vektor 0 0 -1) (:eye-vektor comps)))
      (is (true? (:inside comps)))
      (is (roughly (vektor 0 0 -1) (:normal-vektor comps)))))

  (testing "should offset the point to take imprecise representation of floating point numbers into account"
    (let [ray (ray (point 0 0 -5) (vektor 0 0 1))
          shape (sphere :transform (translation 0 0 1))
          intersection (intersection 5 shape)
          comps (prepare-computation intersection ray {})
          [_ _ point-z] (:point comps)
          [_ _ over-point-z] (:over-point comps)]
      (is (< over-point-z (- (/ epsilon 2))))
      (is (> point-z over-point-z))))

  (testing "should precompute under point as offset below surface"
    (let [ray (ray (point 0 0 -5) (vektor 0 0 1))
          shape (glass-sphere :transform (translation 0 0 1))
          intersection (intersection 5 shape)
          intersections [intersection]
          comps (prepare-computation intersection ray intersections {})
          [_ _ point-z] (:point comps)
          [_ _ under-point-z] (:under-point comps)]
      (is (> under-point-z (/ epsilon 2)))
      (is (< point-z under-point-z))))

  (testing "should be able to precompute reflection vector"
    (let [ray (ray (point 0 1 -1) (vektor 0 (- (/ (sqrt 2) 2)) (/ (sqrt 2) 2)))
          shape (plane)
          intersection (intersection (sqrt 2) shape)
          comps (prepare-computation intersection ray {})]
      (is (roughly (vektor 0 (/ (sqrt 2) 2) (/ (sqrt 2) 2)) (:reflect-vektor comps)))))

  (testing "should find n1 and n2 at various intersections"
    (let [a (assoc-in (glass-sphere :transform (scaling 2 2 2)) [:material :refractive-index] 1.5)
          b (assoc-in (glass-sphere :transform (translation 0 0 -0.25)) [:material :refractive-index] 2.0)
          c (assoc-in (glass-sphere :transform (translation 0 0 0.25)) [:material :refractive-index] 2.5)
          ray (ray (point 0 0 -4) (vektor 0 0 1))
          intersections (mapv (partial apply intersection) [[2 a] [2.75 b] [3.25 c] [4.75 b] [5.25 c] [6 a]])]
      (are [index n1 n2] (let [comps (prepare-computation (intersections index) ray intersections {})]
                           (is (roughly [n1 n2] (mapv comps [:n1 :n2]))))
                         0 1.0 1.5
                         1 1.5 2.0
                         2 2.0 2.5
                         3 2.5 2.5
                         4 2.5 1.5
                         5 1.5 1.0)))

  (testing "should store shape-to-parent mapping"
    (let [ray (ray (point 0 0 -5) (vektor 0 0 1))
          shape (sphere)
          intersection (intersection 4 shape)
          shape-to-parent {(:uuid shape) (sphere)}
          comps (prepare-computation intersection ray shape-to-parent)]
      (is (= shape-to-parent (:shape-to-parent comps))))))

(deftest about-schlick-approximation

  (testing "should approximate under total internal reflection"
    (let [shape (glass-sphere)
          ray (ray (point 0 0 (/ (sqrt 2) 2)) (vektor 0 1 0))
          intersections [(intersection (- (/ (sqrt 2) 2)) shape) (intersection (/ (sqrt 2) 2) shape)]
          comps (prepare-computation (intersections 1) ray intersections {})]
      (is (roughly 1.0 (schlick comps)))))

  (testing "should approximate with a perpendicular viewing angle"
    (let [shape (glass-sphere)
          ray (ray (point 0 0 0) (vektor 0 1 0))
          intersections [(intersection -1 shape) (intersection 1 shape)]
          comps (prepare-computation (intersections 1) ray intersections {})]
      (is (roughly 0.04 (schlick comps)))))

  (testing "should approximate when n2 > n1"
    (let [shape (glass-sphere)
          ray (ray (point 0 0.99 -2) (vektor 0 0 1))
          intersections [(intersection 1.8589 shape)]
          comps (prepare-computation (intersections 0) ray intersections {})]
      (is (roughly 0.48873 (schlick comps))))))
