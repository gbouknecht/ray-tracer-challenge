(ns ray-tracer-challenge.logic.shapes-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(def ^:private saved-args (atom nil))
(def ^:private saved-result (atom nil))

(defn test-shape [& {:keys [transform material]}]
  (letfn [(local-intersect [shape local-ray]
            (reset! saved-args [shape local-ray])
            (reset! saved-result [(intersection 1 shape) (intersection 2 shape)]))
          (local-normal-at [shape local-point]
            (reset! saved-args [shape local-point])
            (let [[x y z] local-point] (vektor x y z)))]
    (shape :test-shape transform material local-intersect local-normal-at)))

(deftest about-shapes

  (testing "should have identity matrix as default transform"
    (is (= identity-matrix (:transform (test-shape)))))

  (testing "should have a default material"
    (is (= (material) (:material (test-shape)))))

  (testing "should be able to set individual components"
    (let [transform (translation 2 3 4)
          material (material :ambient 1)
          shape (test-shape :transform transform :material material)]
      (is (roughly transform (:transform shape)))
      (is (= material (:material shape)))))

  (testing "should be able to calculate intersection on a scaled shape"
    (let [ray (ray (point 0 0 -5) (vektor 0 0 1))
          shape (test-shape :transform (scaling 2 2 2))
          xs (intersect shape ray)
          [saved-shape saved-local-ray] @saved-args]
      (is (= shape saved-shape))
      (is (roughly (point 0 0 -2.5) (:origin saved-local-ray)))
      (is (roughly (vektor 0 0 0.5) (:direction saved-local-ray)))
      (is (= @saved-result xs))))

  (testing "should be able to calculate intersection on a translated shape"
    (let [ray (ray (point 0 0 -5) (vektor 0 0 1))
          shape (test-shape :transform (translation 5 0 0))
          xs (intersect shape ray)
          [saved-shape saved-local-ray] @saved-args]
      (is (= shape saved-shape))
      (is (roughly (point -5 0 -5) (:origin saved-local-ray)))
      (is (roughly (vektor 0 0 1) (:direction saved-local-ray)))
      (is (= @saved-result xs))))

  (testing "should be able to calculate normal on a translated shape"
    (let [shape (test-shape :transform (translation 0 1 0))
          normal (normal-at shape (point 0 1.70711 -0.70711))
          [saved-shape _] @saved-args]
      (is (= shape saved-shape))
      (is (roughly (vektor 0 0.70711 -0.70711) normal))))

  (testing "should be able to calculate normal on a transformed shape"
    (let [shape (test-shape :transform (multiply-matrices (scaling 1 0.5 1) (rotation-z (/ Math/PI 5))))
          normal (normal-at shape (point 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2))))
          [saved-shape _] @saved-args]
      (is (= shape saved-shape))
      (is (roughly (vektor 0 0.97014 -0.24254) normal)))))
