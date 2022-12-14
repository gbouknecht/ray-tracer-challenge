(ns ray-tracer-challenge.logic.spheres-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-spheres

  (testing "should be able to calculate normal"
    (let [sphere (sphere)
          a (/ (sqrt 3) 3)]
      (are [x p] (roughly x (local-normal-at sphere p nil))
                 (vektor 1 0 0) (point 1 0 0)
                 (vektor 0 1 0) (point 0 1 0)
                 (vektor 0 0 1) (point 0 0 1)
                 (vektor a a a) (point a a a)
                 (normalize-vektor (vektor a a a)) (point a a a))))

  (testing "should be able to calculate normal on a translated sphere"
    (let [sphere (sphere :transform (translation 0 1 0))]
      (is (roughly (vektor 0 0.70711 -0.70711) (local-normal-at sphere (point 0 0.70711 -0.70711) nil))))
    (let [sphere (sphere :transform (multiply-matrices (scaling 1 0.5 1) (rotation-z (/ Math/PI 5))))]
      (is (roughly (vektor 0 0.70711 -0.70711) (local-normal-at sphere (point 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2))) nil)))))

  (testing "should be able to calculate intersection"
    (let [sphere (sphere)]
      (are [xs origin] (roughly (mapv #(intersection % sphere) xs) (local-intersect sphere (ray origin (vektor 0 0 1))))
                       [4.0 6.0] (point 0 0 -5)
                       [5.0 5.0] (point 0 1 -5)
                       [] (point 0 2 -5)
                       [-1.0 1.0] (point 0 0 0)
                       [-6.0 -4.0] (point 0 0 5))))

  (testing "should be able to calculate intersection on a scaled sphere"
    (let [local-ray (ray (point 0 0 -2.5) (vektor 0 0 0.5))
          sphere (sphere :transform (scaling 2 2 2))
          xs (local-intersect sphere local-ray)]
      (is (roughly [3 7] (mapv :t xs)))))

  (testing "should be able to calculate intersection on a translated sphere"
    (let [local-ray (ray (point -5 0 -5) (vektor 0 0 1))
          sphere (sphere :transform (translation 5 0 0))
          xs (local-intersect sphere local-ray)]
      (is (roughly [] (mapv :t xs)))))

  (testing "should have a helper for producing a sphere with a glassy material"
    (let [sphere (glass-sphere)]
      (is (= 1.0 (-> sphere :material :transparency)))
      (is (= 1.5 (-> sphere :material :refractive-index))))))
