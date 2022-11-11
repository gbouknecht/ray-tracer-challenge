(ns ray-tracer-challenge.logic.transformations-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-translations

  (testing "should translate points"
    (let [transform (translation 5 -3 2)
          inverse-transform (inverse transform)
          p (point -3 4 5)]
      (is (roughly (point 2 1 7) (multiply-matrix-by-tuple transform p)))
      (is (roughly (point -8 7 3) (multiply-matrix-by-tuple inverse-transform p)))))

  (testing "should not affect vectors"
    (let [transform (translation 5 -3 2)
          v (vektor -3 4 5)]
      (is (roughly v (multiply-matrix-by-tuple transform v))))))

(deftest about-scalings

  (testing "should scale points"
    (let [transform (scaling 2 3 4)
          inverse-transform (inverse transform)
          p (point -4 6 8)]
      (is (roughly (point -8 18 32) (multiply-matrix-by-tuple transform p)))
      (is (roughly (point -2 2 2) (multiply-matrix-by-tuple inverse-transform p)))))

  (testing "should scale vectors"
    (let [transform (scaling 2 3 4)
          inverse-transform (inverse transform)
          v (vektor -4 6 8)]
      (is (roughly (vektor -8 18 32) (multiply-matrix-by-tuple transform v)))
      (is (roughly (vektor -2 2 2) (multiply-matrix-by-tuple inverse-transform v)))))

  (testing "should be able to reflect a point"
    (let [transform (scaling -1 1 1)
          p (point 2 3 4)]
      (is (roughly (point -2 3 4) (multiply-matrix-by-tuple transform p))))))

(deftest about-rotations

  (testing "should rotate around x axis"
    (let [p (point 0 1 0)
          half-quarter (rotation-x (/ Math/PI 4))
          inverse-half-quarter (inverse half-quarter)
          full-quarter (rotation-x (/ Math/PI 2))]
      (is (roughly (point 0 (/ (sqrt 2) 2) (/ (sqrt 2) 2)) (multiply-matrix-by-tuple half-quarter p)))
      (is (roughly (point 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2))) (multiply-matrix-by-tuple inverse-half-quarter p)))
      (is (roughly (point 0 0 1) (multiply-matrix-by-tuple full-quarter p)))))

  (testing "should rotate around y axis"
    (let [p (point 0 0 1)
          half-quarter (rotation-y (/ Math/PI 4))
          full-quarter (rotation-y (/ Math/PI 2))]
      (is (roughly (point (/ (sqrt 2) 2) 0 (/ (sqrt 2) 2)) (multiply-matrix-by-tuple half-quarter p)))
      (is (roughly (point 1 0 0) (multiply-matrix-by-tuple full-quarter p)))))

  (testing "should rotate around z axis"
    (let [p (point 0 1 0)
          half-quarter (rotation-z (/ Math/PI 4))
          full-quarter (rotation-z (/ Math/PI 2))]
      (is (roughly (point (- (/ (sqrt 2) 2)) (/ (sqrt 2) 2) 0) (multiply-matrix-by-tuple half-quarter p)))
      (is (roughly (point -1 0 0) (multiply-matrix-by-tuple full-quarter p))))))

(deftest about-shearing

  (testing "should be able to move each component of a tuple in proportion to the other two components"
    (are [x transform] (roughly x (multiply-matrix-by-tuple transform (point 2 3 4)))
                       (point 5 3 4) (shearing 1 0 0 0 0 0)
                       (point 6 3 4) (shearing 0 1 0 0 0 0)
                       (point 2 5 4) (shearing 0 0 1 0 0 0)
                       (point 2 7 4) (shearing 0 0 0 1 0 0)
                       (point 2 3 6) (shearing 0 0 0 0 1 0)
                       (point 2 3 7) (shearing 0 0 0 0 0 1))))

(deftest about-chaining-transformations

  (testing "should be applied in reverse order"
    (let [p1 (point 1 0 1)
          matrix-a (rotation-x (/ Math/PI 2))
          matrix-b (scaling 5 5 5)
          matrix-c (translation 10 5 7)
          matrix-t (reduce multiply-matrices [matrix-c matrix-b matrix-a])
          p2 (multiply-matrix-by-tuple matrix-a p1)
          p3 (multiply-matrix-by-tuple matrix-b p2)
          p4 (multiply-matrix-by-tuple matrix-c p3)]
      (is (roughly (point 1 -1 0) p2))
      (is (roughly (point 5 -5 0) p3))
      (is (roughly (point 15 0 7) p4))
      (is (roughly p4 (multiply-matrix-by-tuple matrix-t p1))))))

(deftest about-view-transformations

  (testing "should be identity matrix for default orientation"
    (let [from (point 0 0 0)
          to (point 0 0 -1)
          up (vektor 0 1 0)]
      (is (roughly identity-matrix (view-transform from to up)))))

  (testing "should reflect across x and z axes when looking in positive z direction"
    (let [from (point 0 0 0)
          to (point 0 0 1)
          up (vektor 0 1 0)]
      (is (roughly (scaling -1 1 -1) (view-transform from to up)))))

  (testing "should move the world"
    (let [from (point 0 0 8)
          to (point 0 0 0)
          up (vektor 0 1 0)]
      (is (roughly (translation 0 0 -8) (view-transform from to up)))))

  (testing "should be able to look in some arbitrary direction"
    (let [from (point 1 3 2)
          to (point 4 -2 8)
          up (vektor 1 1 0)]
      (is (roughly (matrix [[-0.50709 0.50709 0.67612 -2.36643]
                            [0.76772 0.60609 0.12122 -2.82843]
                            [-0.35857 0.59761 -0.71714 0.00000]
                            [0.00000 0.00000 0.00000 1.00000]])
                   (view-transform from to up))))))
