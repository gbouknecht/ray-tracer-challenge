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
      (is (= 200.0 (:shininess material)))
      (is (= 0.0 (:reflective material)))
      (is (= 0.0 (:transparency material)))
      (is (= 1.0 (:refractive-index material)))))

  (testing "should be able to set individual components"
    (let [color red
          ambient 0.2
          diffuse 0.8
          specular 0.7
          shininess 190
          reflective 0.7
          transparency 1.0
          refractive-index 1.5
          material (material :color color
                             :ambient ambient
                             :diffuse diffuse
                             :specular specular
                             :shininess shininess
                             :reflective reflective
                             :transparency transparency
                             :refractive-index refractive-index)]
      (is (= color (:color material)))
      (is (= ambient (:ambient material)))
      (is (= diffuse (:diffuse material)))
      (is (= specular (:specular material)))
      (is (= shininess (:shininess material)))
      (is (= reflective (:reflective material)))
      (is (= transparency (:transparency material)))
      (is (= refractive-index (:refractive-index material)))))

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
