(ns ray-tracer-challenge.logic.spheres-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-spheres

  (testing "should be able to associate a transformation to a sphere"
    (let [sphere1 (sphere)
          transform (translation 2 3 4)
          sphere2 (set-transform sphere1 transform)]
      (is (roughly identity-matrix (:transform sphere1)))
      (is (roughly transform (:transform sphere2)))))

  (testing "should be able to calculate normal on a sphere"
    (let [sphere (sphere)
          a (/ (sqrt 3) 3)]
      (are [x p] (roughly x (normal-at sphere p))
                 (vektor 1 0 0) (point 1 0 0)
                 (vektor 0 1 0) (point 0 1 0)
                 (vektor 0 0 1) (point 0 0 1)
                 (vektor a a a) (point a a a)
                 (normalize-vektor (vektor a a a)) (point a a a))))

  (testing "should be able to calculate normal on a translated sphere"
    (let [sphere (set-transform (sphere) (translation 0 1 0))]
      (is (roughly (vektor 0 0.70711 -0.70711) (normal-at sphere (point 0 1.70711 -0.70711)))))
    (let [sphere (set-transform (sphere) (multiply-matrices (scaling 1 0.5 1) (rotation-z (/ Math/PI 5))))]
      (is (roughly (vektor 0 0.97014 -0.24254) (normal-at sphere (point 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2)))))))))
