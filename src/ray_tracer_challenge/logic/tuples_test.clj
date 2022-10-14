(ns ray-tracer-challenge.logic.tuples-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-tuples

  (testing
    "should be a point when fourth component is 1.0"
    (let [a [4.3 -4.2 3.1 1.0]]
      (is (roughly (a 0) 4.3))
      (is (roughly (a 1) -4.2))
      (is (roughly (a 2) 3.1))
      (is (roughly (a 3) 1.0))
      (is (point? a))
      (is (not (vektor? a)))))

  (testing
    "should be a vector when fourth component is 0.0"
    (let [a [4.3 -4.2 3.1 0.0]]
      (is (roughly (a 0) 4.3))
      (is (roughly (a 1) -4.2))
      (is (roughly (a 2) 3.1))
      (is (roughly (a 3) 0.0))
      (is (not (point? a)))
      (is (vektor? a))))

  (testing
    "should have 1.0 as fourth component when created by (point x y z)"
    (is (roughly (point 4 -4 3) [4.0 -4.0 3.0 1.0])))

  (testing
    "should have 0.0 as fourth component when created by (vektor x y z)"
    (is (roughly (vektor 4 -4 3) [4.0 -4.0 3.0 0.0]))))

(deftest about-tuple-operations

  (testing
    "should be able to add"
    (is (roughly (add-tuples [3 -2 5 1] [-2 3 1 0]) [1 1 6 1])))

  (testing
    "should be able to subtract two points"
    (is (roughly (subtract-tuples (point 3 2 1) (point 5 6 7)) (vektor -2 -4 -6))))

  (testing
    "should be able to subtract a vector from a point"
    (is (roughly (subtract-tuples (point 3 2 1) (vektor 5 6 7)) (point -2 -4 -6))))

  (testing
    "should be able to subtract two vectors"
    (is (roughly (subtract-tuples (vektor 3 2 1) (vektor 5 6 7)) (vektor -2 -4 -6))))

  (testing
    "should be able to negate"
    (is (roughly (negate-tuple [1 -2 3 -4]) [-1 2 -3 4])))

  (testing
    "should be able to multiply a tuple by a scalar"
    (is (roughly (multiply-tuple [1 -2 3 -4] 3.5) [3.5 -7 10.5 -14])))

  (testing
    "should be able to multiply a tuple by a fraction"
    (is (roughly (multiply-tuple [1 -2 3 -4] 0.5) [0.5 -1 1.5 -2])))

  (testing
    "should be able to divide a tuple by a scalar"
    (is (roughly (divide-tuple [1 -2 3 -4] 2) [0.5 -1 1.5 -2])))

  (testing
    "should be able to compute the magnitude of a vector"
    (is (roughly (magnitude-vektor (vektor 1 0 0)) 1))
    (is (roughly (magnitude-vektor (vektor 0 1 0)) 1))
    (is (roughly (magnitude-vektor (vektor 0 0 1)) 1))
    (is (roughly (magnitude-vektor (vektor 1 2 3)) (sqrt 14)))
    (is (roughly (magnitude-vektor (vektor -1 -2 -3)) (sqrt 14))))

  (testing
    "should be able to normalize a vector"
    (is (roughly (normalize-vektor (vektor 4 0 0)) (vektor 1 0 0)))
    (is (roughly (normalize-vektor (vektor 1 2 3)) (vektor 0.26726 0.53452 0.80178)))
    (is (roughly (magnitude-vektor (normalize-vektor (vektor 1 2 3))) 1)))

  (testing
    "should be able to compute dot product of two tuples"
    (is (roughly (dot-product-tuples (vektor 1 2 3) (vektor 2 3 4)) 20)))

  (testing
    "should be able to compute cross product of two vectors"
    (let [a (vektor 1 2 3)
          b (vektor 2 3 4)]
      (is (roughly (cross-product-vektors a b) (vektor -1 2 -1)))
      (is (roughly (cross-product-vektors b a) (vektor 1 -2 1))))))
