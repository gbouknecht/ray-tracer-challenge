(ns ray-tracer-challenge.logic.patterns-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.patterns :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(def ^:private saved-args (atom nil))

(defn test-pattern [& {:keys [transform]}]
  (letfn [(pattern-at [pattern [x y z :as point]]
            (reset! saved-args [pattern point])
            (color x y z))]
    (pattern transform pattern-at)))

(deftest about-patterns

  (testing "should have identity matrix as default transform"
    (is (= identity-matrix (:transform (test-pattern)))))

  (testing "should be able to set a transformation"
    (let [transform (translation 1 2 3)
          pattern (test-pattern :transform transform)]
      (is (roughly transform (:transform pattern)))))

  (testing "should respect object transformation"
    (let [pattern (test-pattern)
          shape (sphere :transform (scaling 2 2 2))
          actual-color (pattern-at-shape pattern shape (point 2 3 4))
          [saved-pattern _] @saved-args]
      (is (= pattern saved-pattern))
      (is (roughly (color 1 1.5 2) actual-color))))

  (testing "should respect pattern transformation"
    (let [pattern (test-pattern :transform (scaling 2 2 2))
          shape (sphere)
          actual-color (pattern-at-shape pattern shape (point 2 3 4))
          [saved-pattern _] @saved-args]
      (is (= pattern saved-pattern))
      (is (roughly (color 1 1.5 2) actual-color))))

  (testing "should respect both object and pattern transformation"
    (let [pattern (test-pattern :transform (translation 0.5 1 1.5))
          shape (sphere :transform (scaling 2 2 2))
          actual-color (pattern-at-shape pattern shape (point 2.5 3 3.5))
          [saved-pattern _] @saved-args]
      (is (= pattern saved-pattern))
      (is (roughly (color 0.75 0.5 0.25) actual-color)))))

(deftest about-stripe-patterns

  (testing "should be able to create a stripe pattern"
    (let [pattern (stripe-pattern white black)]
      (is (roughly white (:a pattern)))
      (is (roughly black (:b pattern)))))

  (testing "should be constant in y"
    (let [pattern (stripe-pattern white black)]
      (is (roughly white (pattern-at pattern (point 0 0 0))))
      (is (roughly white (pattern-at pattern (point 0 1 0))))
      (is (roughly white (pattern-at pattern (point 0 2 0))))))

  (testing "should be constant in z"
    (let [pattern (stripe-pattern white black)]
      (is (roughly white (pattern-at pattern (point 0 0 0))))
      (is (roughly white (pattern-at pattern (point 0 0 1))))
      (is (roughly white (pattern-at pattern (point 0 0 2))))))

  (testing "should alternate in x"
    (let [pattern (stripe-pattern white black)]
      (is (roughly white (pattern-at pattern (point 0 0 0))))
      (is (roughly white (pattern-at pattern (point 0.9 0 0))))
      (is (roughly black (pattern-at pattern (point 1 0 0))))
      (is (roughly black (pattern-at pattern (point -0.1 0 0))))
      (is (roughly black (pattern-at pattern (point -1 0 0))))
      (is (roughly white (pattern-at pattern (point -1.1 0 0)))))))

(deftest about-gradient-patterns

  (testing "should be able to create a gradient pattern"
    (let [pattern (gradient-pattern white black)]
      (is (roughly white (:a pattern)))
      (is (roughly black (:b pattern)))))

  (testing "should linearly interpolate between colors"
    (let [pattern (gradient-pattern white black)]
      (is (roughly white (pattern-at pattern (point 0 0 0))))
      (is (roughly (color 0.75 0.75 0.75) (pattern-at pattern (point 0.25 0 0))))
      (is (roughly (color 0.5 0.5 0.5) (pattern-at pattern (point 0.5 0 0))))
      (is (roughly (color 0.25 0.25 0.25) (pattern-at pattern (point 0.75 0 0)))))))

(deftest about-ring-patterns

  (testing "should be able to create a ring pattern"
    (let [pattern (ring-pattern white black)]
      (is (roughly white (:a pattern)))
      (is (roughly black (:b pattern)))))

  (testing "should extend ring in both x and z"
    (let [pattern (ring-pattern white black)]
      (is (roughly white (pattern-at pattern (point 0 0 0))))
      (is (roughly black (pattern-at pattern (point 1 0 0))))
      (is (roughly black (pattern-at pattern (point 0 0 1))))
      (is (roughly black (pattern-at pattern (point 0.708 0 0.708)))))))

(deftest about-checkers-patterns

  (testing "should be able to create a checkers pattern"
    (let [pattern (checkers-pattern white black)]
      (is (roughly white (:a pattern)))
      (is (roughly black (:b pattern)))))

  (testing "should repeat in x"
    (let [pattern (checkers-pattern white black)]
      (is (roughly white (pattern-at pattern (point 0 0 0))))
      (is (roughly white (pattern-at pattern (point 0.99 0 0))))
      (is (roughly black (pattern-at pattern (point 1.01 0 0))))))

  (testing "should repeat in y"
    (let [pattern (checkers-pattern white black)]
      (is (roughly white (pattern-at pattern (point 0 0 0))))
      (is (roughly white (pattern-at pattern (point 0 0.99 0))))
      (is (roughly black (pattern-at pattern (point 0 1.01 0))))))

  (testing "should repeat in z"
    (let [pattern (checkers-pattern white black)]
      (is (roughly white (pattern-at pattern (point 0 0 0))))
      (is (roughly white (pattern-at pattern (point 0 0 0.99))))
      (is (roughly black (pattern-at pattern (point 0 0 1.01)))))))
