(ns ray-tracer-challenge.logic.cubes-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.cubes :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-cubes

  (testing "should be able to calculate intersection"
    (let [cube (cube)]
      (are [xs origin direction] (roughly (mapv #(intersection % cube) xs) (local-intersect cube (ray origin direction)))
                                 [4 6] (point 5 0.5 0) (vektor -1 0 0)
                                 [4 6] (point -5 0.5 0) (vektor 1 0 0)
                                 [4 6] (point 0.5 5 0) (vektor 0 -1 0)
                                 [4 6] (point 0.5 -5 0) (vektor 0 1 0)
                                 [4 6] (point 0.5 0 5) (vektor 0 0 -1)
                                 [4 6] (point 0 0.5 -5) (vektor 0 0 1)
                                 [-1 1] (point 0 0.5 0) (vektor 0 0 1))))

  (testing "should have no intersection with rays that misses"
    (let [cube (cube)]
      (are [origin direction] (empty? (local-intersect cube (ray origin direction)))
                              (point -2, 0, 0) (vektor 0.2673, 0.5345, 0.8018)
                              (point 0, -2, 0) (vektor 0.8018, 0.2673, 0.5345)
                              (point 0, 0, -2) (vektor 0.5345, 0.8018, 0.2673)
                              (point 2, 0, 2) (vektor 0, 0, -1)
                              (point 0, 2, 2) (vektor 0, -1, 0)
                              (point 2, 2, 0) (vektor -1, 0, 0))))

  (testing "should be able to calculate normal"
    (let [cube (cube)]
      (are [normal point] (roughly normal (local-normal-at cube point))
                          (vektor 1, 0, 0) (point 1, 0.5, -0.8)
                          (vektor -1, 0, 0) (point -1, -0.2, 0.9)
                          (vektor 0, 1, 0) (point -0.4, 1, -0.1)
                          (vektor 0, -1, 0) (point 0.3, -1, -0.7)
                          (vektor 0, 0, 1) (point -0.6, 0.3, 1)
                          (vektor 0, 0, -1) (point 0.4, 0.4, -1)
                          (vektor 1, 0, 0) (point 1, 1, 1)
                          (vektor -1, 0, 0) (point -1, -1, -1)))))
