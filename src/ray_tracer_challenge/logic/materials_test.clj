(ns ray-tracer-challenge.logic.materials-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.patterns :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-materials

  (testing "should have reasonable defaults"
    (let [material (material)]
      (is (= white (:color material)))
      (is (= 0.1 (:ambient material)))
      (is (= 0.9 (:diffuse material)))
      (is (= 0.9 (:specular material)))
      (is (= 200.0 (:shininess material)))))

  (testing "should be able to set individual components"
    (let [color red
          ambient 0.2
          diffuse 0.8
          specular 0.7
          shininess 190
          material (material :color color :ambient ambient :diffuse diffuse :specular specular :shininess shininess)]
      (is (= color (:color material)))
      (is (= ambient (:ambient material)))
      (is (= diffuse (:diffuse material)))
      (is (= specular (:specular material)))
      (is (= shininess (:shininess material)))))

  (testing "should be able to have a pattern"
    (let [material (material :pattern (stripe-pattern (color 1 1 1) (color 0 0 0))
                             :ambient 1
                             :diffuse 0
                             :specular 0)
          eye-vektor (vektor 0 0 -1)
          normal-vektor (vektor 0 0 -1)
          light (point-light (point 0 0 -10) (color 1 1 1))]
      (is (roughly (color 1 1 1) (lighting material (sphere) light (point 0.9 0 0) eye-vektor normal-vektor false)))
      (is (roughly (color 0 0 0) (lighting material (sphere) light (point 1.1 0 0) eye-vektor normal-vektor false))))))
