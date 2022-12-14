(ns ray-tracer-challenge.logic.planes-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.planes :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-planes

  (testing "should have a constant normal everywhere"
    (let [plane (plane)]
      (is (roughly (vektor 0 1 0) (local-normal-at plane (point 0 0 0) nil)))
      (is (roughly (vektor 0 1 0) (local-normal-at plane (point 10 0 -10) nil)))
      (is (roughly (vektor 0 1 0) (local-normal-at plane (point -5 0 150) nil)))))

  (testing "should have no intersection with a parallel ray"
    (let [plane (plane)
          ray (ray (point 0 10 0) (vektor 0 0 1))]
      (is (empty? (local-intersect plane ray)))))

  (testing "should have no intersection with a coplanar ray"
    (let [plane (plane)
          ray (ray (point 0 0 0) (vektor 0 0 1))]
      (is (empty? (local-intersect plane ray)))))

  (testing "should be able to calculate intersection with ray from above"
    (let [plane (plane)
          ray (ray (point 0 1 0) (vektor 0 -1 0))
          xs (local-intersect plane ray)]
      (is (= (count xs) 1))
      (is (roughly 1 (:t (first xs))))
      (is (= plane (:object (first xs))))))

  (testing "should be able to calculate intersection with ray from below"
    (let [plane (plane)
          ray (ray (point 0 -1 0) (vektor 0 1 0))
          xs (local-intersect plane ray)]
      (is (= (count xs) 1))
      (is (roughly 1 (:t (first xs))))
      (is (= plane (:object (first xs)))))))
