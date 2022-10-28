(ns ray-tracer-challenge.logic.rays-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-rays

  (testing
    "should be able to create a ray"
    (let [origin (point 1 2 3)
          direction (vektor 4 5 6)
          ray (ray origin direction)]
      (is (= origin (:origin ray)))
      (is (= direction (:direction ray)))))

  (testing
    "should be able to calculate position at a given distant"
    (let [ray (ray (point 2 3 4) (vektor 1 0 0))]
      (is (roughly (point 2 3 4) (position ray 0)))
      (is (roughly (point 3 3 4) (position ray 1)))
      (is (roughly (point 1 3 4) (position ray -1)))
      (is (roughly (point 4.5 3 4) (position ray 2.5)))))

  (testing
    "should be able to calculate intersection with a sphere"
    (let [sphere (sphere)]
      (are [xs origin] (roughly (mapv #(intersection % sphere) xs) (intersect sphere (ray origin (vektor 0 0 1))))
                       [4.0 6.0] (point 0 0 -5)
                       [5.0 5.0] (point 0 1 -5)
                       [] (point 0 2 -5)
                       [-1.0 1.0] (point 0 0 0)
                       [-6.0 -4.0] (point 0 0 5))))

  (testing
    "should be able to encapsulate t and object as an intersection"
    (let [sphere (sphere)
          intersection (intersection 3.5 sphere)]
      (is (= 3.5 (:t intersection)))
      (is (= sphere (:object intersection)))))

  (testing
    "should be able to check if something is an intersection"
    (let [intersection (intersection 3.5 (sphere))]
      (is (intersection? intersection))
      (is (not (intersection? (dissoc intersection :t))))
      (is (not (intersection? (dissoc intersection :object)))))
    (is (not (intersection? [1 2 3])))
    (is (not (intersection? {:a 1 :b 2}))))

  (testing
    "should hit the first intersection with positive t"
    (let [sphere (sphere)]
      (let [intersection1 (intersection 1 sphere)
            intersection2 (intersection 2 sphere)
            intersections [intersection2 intersection1]]
        (is (roughly intersection1 (hit intersections))))
      (let [intersection1 (intersection -1 sphere)
            intersection2 (intersection 1 sphere)
            intersections [intersection1 intersection2]]
        (is (roughly intersection2 (hit intersections))))
      (let [intersection1 (intersection -2 sphere)
            intersection2 (intersection -1 sphere)
            intersections [intersection1 intersection2]]
        (is (nil? (hit intersections))))
      (let [intersection1 (intersection 5 sphere)
            intersection2 (intersection 7 sphere)
            intersection3 (intersection -3 sphere)
            intersection4 (intersection 2 sphere)
            intersections [intersection1 intersection2 intersection3 intersection4]]
        (is (roughly intersection4 (hit intersections))))))

  (testing
    "should be able to translate a ray"
    (let [ray1 (ray (point 1 2 3) (vektor 0 1 0))
          matrix (translation 3 4 5)
          ray2 (transform ray1 matrix)]
      (is (roughly (point 4 6 8) (:origin ray2)))
      (is (roughly (vektor 0 1 0) (:direction ray2)))))

  (testing
    "should be able to scale a ray"
    (let [ray1 (ray (point 1 2 3) (vektor 0 1 0))
          matrix (scaling 2 3 4)
          ray2 (transform ray1 matrix)]
      (is (roughly (point 2 6 12) (:origin ray2)))
      (is (roughly (vektor 0 3 0) (:direction ray2)))))

  (testing
    "should be able to associate a transformation to a sphere"
    (let [sphere1 (sphere)
          transform (translation 2 3 4)
          sphere2 (set-transform sphere1 transform)]
      (is (roughly identity-matrix (:transform sphere1)))
      (is (roughly transform (:transform sphere2)))))

  (testing
    "should be able to intersect a scaled sphere"
    (let [ray (ray (point 0 0 -5) (vektor 0 0 1))
          sphere (set-transform (sphere) (scaling 2 2 2))
          xs (intersect sphere ray)]
      (is (roughly [3 7] (mapv :t xs)))))

  (testing
    "should be able to intersect translated sphere"
    (let [ray (ray (point 0 0 -5) (vektor 0 0 1))
          sphere (set-transform (sphere) (translation 5 0 0))
          xs (intersect sphere ray)]
      (is (roughly [] (mapv :t xs))))))
