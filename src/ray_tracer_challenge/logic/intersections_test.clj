(ns ray-tracer-challenge.logic.intersections-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
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
      (is (roughly (vektor 0 0 -1) (:normal-vektor comps))))))
