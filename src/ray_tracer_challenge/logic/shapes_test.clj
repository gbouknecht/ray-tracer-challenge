(ns ray-tracer-challenge.logic.shapes-test
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(def ^:private saved-args (atom nil))
(def ^:private saved-result (atom nil))

(defn test-shape [& {:keys [transform material]}]
  (letfn [(local-intersect [shape local-ray]
            (reset! saved-args [shape local-ray])
            (reset! saved-result [(intersection 1 shape) (intersection 2 shape)]))
          (local-normal-at [shape local-point _]
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
          normal (normal-at shape (point 0 1.70711 -0.70711) nil {})
          [saved-shape _] @saved-args]
      (is (= shape saved-shape))
      (is (roughly (vektor 0 0.70711 -0.70711) normal))))

  (testing "should be able to calculate normal on a transformed shape"
    (let [shape (test-shape :transform (multiply-matrices (scaling 1 0.5 1) (rotation-z (/ Math/PI 5))))
          normal (normal-at shape (point 0 (/ (sqrt 2) 2) (- (/ (sqrt 2) 2))) nil {})
          [saved-shape _] @saved-args]
      (is (= shape saved-shape))
      (is (roughly (vektor 0 0.97014 -0.24254) normal)))))

(deftest about-groups

  (testing "should have identity transformation by default and not contains any shapes"
    (let [group (group)]
      (is (roughly identity-matrix (:transform group)))
      (is (empty? (:children group)))
      (is (empty? (shape-to-parent group)))))

  (testing "should be able to set children"
    (let [shape-a1 (test-shape)
          shape-a2 (test-shape)
          group-a (group :children [shape-a1 shape-a2])
          shape-b (test-shape)
          group-b (group :children [shape-b])
          shape-c (test-shape)
          group-c (group :children [shape-c group-a group-b])
          shape-d (test-shape)
          group-d (group :children [shape-d])
          shape (test-shape)]
      (is (some #{shape-a1} (:children group-a)))
      (is (= {(:uuid shape-a1) group-a
              (:uuid shape-a2) group-a
              (:uuid group-a)  group-c
              (:uuid group-b)  group-c
              (:uuid shape-b)  group-b
              (:uuid shape-c)  group-c
              (:uuid shape-d)  group-d} (shape-to-parent [group-c shape group-d])))))

  (testing "should have no intersection when empty"
    (let [group (group)
          ray (ray (point 0 0 0) (vektor 0 0 1))]
      (is (empty? (local-intersect group ray)))))

  (testing "should be able to calculate intersection when nonempty"
    (let [sphere1 (sphere)
          sphere2 (sphere :transform (translation 0 0 -3))
          sphere3 (sphere :transform (translation 5 0 0))
          group (group :children [sphere1 sphere2 sphere3])
          ray (ray (point 0 0 -5) (vektor 0 0 1))
          intersections (local-intersect group ray)]
      (is (= [sphere2 sphere2 sphere1 sphere1] (map :object intersections)))))

  (testing "should be able to calculate intersection on transformed group"
    (let [sphere (sphere :transform (translation 5 0 0))
          group (group :transform (scaling 2 2 2) :children [sphere])
          ray (ray (point 10 0 -10) (vektor 0 0 1))]
      (is (= 2 (count (intersect group ray))))))

  (testing "should be able to calculate normal on transformed group"
    (let [sphere (sphere :transform (translation 5 0 0))
          group2 (group :transform (scaling 1 2 3) :children [sphere])
          group1 (group :transform (rotation-y (/ Math/PI 2)) :children [group2])
          shape-to-parent (shape-to-parent [group1])]
      (is (roughly (vektor 0.28570 0.42854 -0.85716) (normal-at sphere
                                                                (point 1.7321 1.1547 -5.5774)
                                                                nil
                                                                shape-to-parent))))))

(deftest about-world-to-object

  (testing "should convert a point from world to object space"
    (let [sphere (sphere :transform (translation 5 0 0))
          group2 (group :transform (scaling 2 2 2) :children [sphere])
          group1 (group :transform (rotation-y (/ Math/PI 2)) :children [group2])
          shape-to-parent (shape-to-parent [group1])]
      (is (roughly (point 0 0 -1) (world-to-object sphere (point -2 0 -10) shape-to-parent))))))

(deftest about-normal-to-world

  (testing "should convert a normal from object to world space"
    (let [sphere (sphere :transform (translation 5 0 0))
          group2 (group :transform (scaling 1 2 3) :children [sphere])
          group1 (group :transform (rotation-y (/ Math/PI 2)) :children [group2])
          shape-to-parent (shape-to-parent [group1])]
      (is (roughly (vektor 0.28571 0.42857 -0.85714) (normal-to-world sphere
                                                                      (vektor (/ (sqrt 3) 3) (/ (sqrt 3) 3) (/ (sqrt 3) 3))
                                                                      shape-to-parent))))))
