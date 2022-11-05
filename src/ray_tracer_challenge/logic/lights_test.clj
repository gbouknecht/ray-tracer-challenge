(ns ray-tracer-challenge.logic.lights-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-point-lights

  (testing "should has a position and intensity"
    (let [position (point 0 0 0)
          intensity white
          light (point-light position intensity)]
      (is (= position (:position light)))
      (is (= intensity (:intensity light))))))

(deftest about-lighting

  (let [material (material)
        position (point 0 0 0)]

    (testing "should be able to handle eye between light and surface"
      (let [eye-vektor (vektor 0 0 -1)
            normal-vektor (vektor 0 0 -1)
            light (point-light (point 0 0 -10) white)
            result (lighting material light position eye-vektor normal-vektor)]
        (is (roughly (color 1.9 1.9 1.9) result))))

    (testing "should be able to handle eye between light and surface, eye offset 45°"
      (let [eye-vektor (vektor 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2)))
            normal-vektor (vektor 0 0 -1)
            light (point-light (point 0 0 -10) white)
            result (lighting material light position eye-vektor normal-vektor)]
        (is (roughly white result))))

    (testing "should be able to handle eye opposite surface, light offset 45°"
      (let [eye-vektor (vektor 0 0 -1)
            normal-vektor (vektor 0 0 -1)
            light (point-light (point 0 10 -10) white)
            result (lighting material light position eye-vektor normal-vektor)]
        (is (roughly (color 0.7364 0.7364 0.7364) result))))

    (testing "should be able to handle eye in path of reflection vector"
      (let [eye-vektor (vektor 0 (- (/ (sqrt 2) 2)) (- (/ (sqrt 2) 2)))
            normal-vektor (vektor 0 0 -1)
            light (point-light (point 0 10 -10) white)
            result (lighting material light position eye-vektor normal-vektor)]
        (is (roughly (color 1.6364 1.6364 1.6364) result))))

    (testing "should be able to handle light behind surface"
      (let [eye-vektor (vektor 0 0 -1)
            normal-vektor (vektor 0 0 -1)
            light (point-light (point 0 0 10) white)
            result (lighting material light position eye-vektor normal-vektor)]
        (is (roughly (color 0.1 0.1 0.1) result))))))
