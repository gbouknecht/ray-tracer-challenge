(ns ray-tracer-challenge.logic.colors-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-color-operations

  (testing "should be able to add"
    (is (roughly [1.6 0.7 1.0] (add-colors [0.9 0.6 0.75] [0.7 0.1 0.25]))))

  (testing "should be able to subtract"
    (is (roughly [0.2 0.5 0.5] (subtract-colors [0.9 0.6 0.75] [0.7 0.1 0.25]))))

  (testing "should be able to multiply by a scalar"
    (is (roughly [0.4 0.6 0.8] (multiply-color [0.2 0.3 0.4] 2))))

  (testing "should be able to multiply"
    (is (roughly [0.9 0.2 0.04] (multiply-colors [1 0.2 0.4] [0.9 1 0.1])))))
