(ns ray-tracer-challenge.logic.spheres-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-spheres

  (testing "should have identity matrix as default transform"
    (is (= identity-matrix (:transform (sphere)))))

  (testing "should have a default material"
    (is (= (material) (:material (sphere)))))

  (testing "should be able to set individual components"
    (let [transform (translation 2 3 4)
          material (material :ambient 1)
          sphere (sphere :transform transform :material material)]
      (is (roughly transform (:transform sphere)))
      (is (= material (:material sphere)))))

  (testing "should be able to calculate normal"
    (let [sphere (sphere)
          a (/ (sqrt 3) 3)]
      (are [x p] (roughly x (normal-at sphere p))
                 (vektor 1 0 0) (point 1 0 0)
                 (vektor 0 1 0) (point 0 1 0)
                 (vektor 0 0 1) (point 0 0 1)
                 (vektor a a a) (point a a a)
                 (normalize-vektor (vektor a a a)) (point a a a))))

  (testing "should be able to calculate normal on a translated sphere"
    (let [sphere (sphere :transform (translation 0 1 0))]
      (is (roughly (vektor 0 0.70711 -0.70711) (normal-at sphere (point 0 1.70711 -0.70711)))))
    (let [sphere (sphere :transform (multiply-matrices (scaling 1 0.5 1) (rotation-z (/ Math/PI 5))))]
      (is (roughly (vektor 0 0.97014 -0.24254) (normal-at sphere (point 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2)))))))))
