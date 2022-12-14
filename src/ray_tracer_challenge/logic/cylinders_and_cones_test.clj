(ns ray-tracer-challenge.logic.cylinders-and-cones-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.cylinders-and-cones :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-cylinders

  (testing "should have no intersection with rays that misses"
    (let [cylinder (cylinder)]
      (are [origin direction] (empty (local-intersect cylinder (ray origin (normalize-vektor direction))))
                              (point 1 0 0) (vektor 0 1 0)
                              (point 0 0 0) (vektor 0 1 0)
                              (point 0 0 -5) (vektor 1 1 1))))

  (testing "should be able to calculate intersection"
    (let [cylinder (cylinder)]
      (are [xs origin direction] (roughly (mapv #(intersection % cylinder) xs)
                                          (local-intersect cylinder (ray origin (normalize-vektor direction))))
                                 [5 5] (point 1 0 -5) (vektor 0 0 1)
                                 [4 6] (point 0 0 -5) (vektor 0 0 1)
                                 [6.80798 7.08872] (point 0.5 0 -5) (vektor 0.1 1 1))))

  (testing "should be able to calculate normal"
    (let [cylinder (cylinder)]
      (are [normal point] (roughly normal (local-normal-at cylinder point))
                          (vektor 1 0 0) (point 1 0 0)
                          (vektor 0 0 -1) (point 0 5 -1)
                          (vektor 0 0 1) (point 0 -2 1)
                          (vektor -1 0 0) (point -1 1 0))))

  (testing "should have default minimum, maximum and closed value"
    (let [cylinder (cylinder)]
      (is (= ##-Inf (:minimum cylinder)))
      (is (= ##Inf (:maximum cylinder)))
      (is (false? (:closed cylinder)))))

  (testing "should be able to calculate intersection on constrained cylinder"
    (let [cylinder (cylinder :minimum 1 :maximum 2)]
      (are [n origin direction] (= n (count (local-intersect cylinder (ray origin (normalize-vektor direction)))))
                                0 (point 0 1.5 0) (vektor 0.1 1 0)
                                0 (point 0 3 -5) (vektor 0 0 1)
                                0 (point 0 0 -5) (vektor 0 0 1)
                                0 (point 0 2 -5) (vektor 0 0 1)
                                0 (point 0 1 -5) (vektor 0 0 1)
                                2 (point 0 1.5 -2) (vektor 0 0 1))))

  (testing "should be able to calculate intersection on caps of closed cylinder"
    (let [cylinder (cylinder :minimum 1 :maximum 2 :closed true)]
      (are [n origin direction] (= n (count (local-intersect cylinder (ray origin (normalize-vektor direction)))))
                                2 (point 0 3 0) (vektor 0 -1 0)
                                2 (point 0 3 -2) (vektor 0 -1 2)
                                2 (point 0 4 -2) (vektor 0 -1 1)
                                2 (point 0 0 -2) (vektor 0 1 2)
                                2 (point 0 -1 -2) (vektor 0 1 1))))

  (testing "should be able to calculate normal on caps of closed cylinder"
    (let [cylinder (cylinder :minimum 1 :maximum 2 :closed true)]
      (are [normal point] (roughly normal (local-normal-at cylinder point))
                          (vektor 0 -1 0) (point 0 1 0)
                          (vektor 0 -1 0) (point 0.5 1 0)
                          (vektor 0 -1 0) (point 0 1 0.5)
                          (vektor 0 1 0) (point 0 2 0)
                          (vektor 0 1 0) (point 0.5 2 0)
                          (vektor 0 1 0) (point 0 2 0.5)))))

(deftest about-cones

  (testing "should be able to calculate intersection"
    (let [cone (cone)]
      (are [xs origin direction] (roughly (mapv #(intersection % cone) xs)
                                          (local-intersect cone (ray origin (normalize-vektor direction))))
                                 [5 5] (point 0 0 -5) (vektor 0 0 1)
                                 [8.66025 8.66025] (point 0 0 -5) (vektor 1 1 1)
                                 [4.55006 49.44994] (point 1 1 -5) (vektor -0.5 -1 1))))

  (testing "should be able to calculate intersection for ray parallel to one of its halves"
    (let [cone (cone)
          direction (normalize-vektor (vektor 0 1 1))
          ray (ray (point 0 0 -1) direction)]
      (is (roughly [(intersection 0.35355 cone)] (local-intersect cone ray)))))

  (testing "should be able to calculate intersection on caps of closed cone"
    (let [cone (cone :minimum -0.5 :maximum 0.5 :closed true)]
      (are [n origin direction] (= n (count (local-intersect cone (ray origin (normalize-vektor direction)))))
                                0 (point 0 0 -5) (vektor 0 1 0)
                                2 (point 0 0 -0.25) (vektor 0 1 1)
                                4 (point 0 0 -0.25) (vektor 0 1 0))))

  (testing "should be able to calculate normal"
    (let [cone (cone)]
      (are [normal point] (roughly normal (local-normal-at cone point))
                          (vektor 0 0 0) (point 0 0 0)
                          (vektor 1 (- (sqrt 2)) 1) (point 1 1 1)
                          (vektor -1 1 0) (point -1 -1 0)))))
