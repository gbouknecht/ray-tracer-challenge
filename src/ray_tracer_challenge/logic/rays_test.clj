(ns ray-tracer-challenge.logic.rays-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-rays

  (testing "should be able to create a ray"
    (let [origin (point 1 2 3)
          direction (vektor 4 5 6)
          ray (ray origin direction)]
      (is (= origin (:origin ray)))
      (is (= direction (:direction ray)))))

  (testing "should be able to calculate position at a given distant"
    (let [ray (ray (point 2 3 4) (vektor 1 0 0))]
      (is (roughly (point 2 3 4) (position ray 0)))
      (is (roughly (point 3 3 4) (position ray 1)))
      (is (roughly (point 1 3 4) (position ray -1)))
      (is (roughly (point 4.5 3 4) (position ray 2.5)))))

  (testing "should be able to translate a ray"
    (let [ray1 (ray (point 1 2 3) (vektor 0 1 0))
          matrix (translation 3 4 5)
          ray2 (transform ray1 matrix)]
      (is (roughly (point 4 6 8) (:origin ray2)))
      (is (roughly (vektor 0 1 0) (:direction ray2)))))

  (testing "should be able to scale a ray"
    (let [ray1 (ray (point 1 2 3) (vektor 0 1 0))
          matrix (scaling 2 3 4)
          ray2 (transform ray1 matrix)]
      (is (roughly (point 2 6 12) (:origin ray2)))
      (is (roughly (vektor 0 3 0) (:direction ray2))))))
