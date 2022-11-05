(ns ray-tracer-challenge.logic.lights-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(deftest about-point-lights

  (testing "should has a position and intensity"
    (let [position (point 0 0 0)
          intensity (color 1 1 1)
          light (point-light position intensity)]
      (is (= position (:position light)))
      (is (= intensity (:intensity light))))))
