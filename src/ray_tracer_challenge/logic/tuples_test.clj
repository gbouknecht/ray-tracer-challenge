(ns ray-tracer-challenge.logic.tuples-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-tuples

  (testing "should be a point when fourth component is 1.0"
    (let [a [4.3 -4.2 3.1 1.0]]
      (is (roughly 4.3 (a 0)))
      (is (roughly -4.2 (a 1)))
      (is (roughly 3.1 (a 2)))
      (is (roughly 1.0 (a 3)))
      (is (point? a))
      (is (not (vektor? a)))))

  (testing "should be a vector when fourth component is 0.0"
    (let [a [4.3 -4.2 3.1 0.0]]
      (is (roughly 4.3 (a 0)))
      (is (roughly -4.2 (a 1)))
      (is (roughly 3.1 (a 2)))
      (is (roughly 0.0 (a 3)))
      (is (not (point? a)))
      (is (vektor? a))))

  (testing "should have 1.0 as fourth component when created by (point x y z)"
    (is (roughly [4.0 -4.0 3.0 1.0] (point 4 -4 3))))

  (testing "should have 0.0 as fourth component when created by (vektor x y z)"
    (is (roughly [4.0 -4.0 3.0 0.0] (vektor 4 -4 3)))))

(deftest about-tuple-operations

  (testing "should be able to add"
    (is (roughly [1 1 6 1] (add-tuples [3 -2 5 1] [-2 3 1 0]))))

  (testing "should be able to subtract two points"
    (is (roughly (vektor -2 -4 -6) (subtract-tuples (point 3 2 1) (point 5 6 7)))))

  (testing "should be able to subtract a vector from a point"
    (is (roughly (point -2 -4 -6) (subtract-tuples (point 3 2 1) (vektor 5 6 7)))))

  (testing "should be able to subtract two vectors"
    (is (roughly (vektor -2 -4 -6) (subtract-tuples (vektor 3 2 1) (vektor 5 6 7)))))

  (testing "should be able to negate"
    (is (roughly [-1 2 -3 4] (negate-tuple [1 -2 3 -4]))))

  (testing "should be able to multiply a tuple by a scalar"
    (is (roughly [3.5 -7 10.5 -14] (multiply-tuple [1 -2 3 -4] 3.5))))

  (testing "should be able to multiply a tuple by a fraction"
    (is (roughly [0.5 -1 1.5 -2] (multiply-tuple [1 -2 3 -4] 0.5))))

  (testing "should be able to divide a tuple by a scalar"
    (is (roughly [0.5 -1 1.5 -2] (divide-tuple [1 -2 3 -4] 2))))

  (testing "should be able to compute the magnitude of a vector"
    (is (roughly 1 (magnitude-vektor (vektor 1 0 0))))
    (is (roughly 1 (magnitude-vektor (vektor 0 1 0))))
    (is (roughly 1 (magnitude-vektor (vektor 0 0 1))))
    (is (roughly (sqrt 14) (magnitude-vektor (vektor 1 2 3))))
    (is (roughly (sqrt 14) (magnitude-vektor (vektor -1 -2 -3)))))

  (testing "should be able to normalize a vector"
    (is (roughly (vektor 1 0 0) (normalize-vektor (vektor 4 0 0))))
    (is (roughly (vektor 0.26726 0.53452 0.80178) (normalize-vektor (vektor 1 2 3))))
    (is (roughly 1 (magnitude-vektor (normalize-vektor (vektor 1 2 3))))))

  (testing "should be able to compute dot product of two tuples"
    (is (roughly 20 (dot-product-tuples (vektor 1 2 3) (vektor 2 3 4)))))

  (testing "should be able to compute cross product of two vectors"
    (let [a (vektor 1 2 3)
          b (vektor 2 3 4)]
      (is (roughly (vektor -1 2 -1) (cross-product-vektors a b)))
      (is (roughly (vektor 1 -2 1) (cross-product-vektors b a)))))

  (testing "should be able to reflect a vektor"
    (is (roughly (vektor 1 1 0) (reflect (vektor 1 -1 0) (vektor 0 1 0))))
    (is (roughly (vektor 1 0 0) (reflect (vektor 0 -1 0) (vektor (/ (sqrt 2) 2) (/ (sqrt 2) 2) 0))))))
