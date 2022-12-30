(ns ray-tracer-challenge.logic.csg-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.csg :refer :all]
            [ray-tracer-challenge.logic.cubes :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-csg

  (testing "should consist of an operation and two shapes"
    (let [shape1 (sphere)
          shape2 (cube)
          csg (csg :union shape1 shape2)]
      (is (= :union (:operation csg)))
      (is (= shape1 (:left csg)))
      (is (= shape2 (:right csg)))))

  (testing "should implement CSG operations"
    (are [operation left-hit inside-left inside-right result]
      (= result (intersection-allowed? operation left-hit inside-left inside-right))
      :union true true true false
      :union true true false true
      :union true false true false
      :union true false false true
      :union false true true false
      :union false true false false
      :union false false true true
      :union false false false true
      :intersection true true true true
      :intersection true true false false
      :intersection true false true true
      :intersection true false false false
      :intersection false true true true
      :intersection false true false true
      :intersection false false true false
      :intersection false false false false
      :difference true true true false
      :difference true true false true
      :difference true false true false
      :difference true false false true
      :difference false true true true
      :difference false true false true
      :difference false false true false
      :difference false false false false))

  (testing "should be able to filter a list of intersections"
    (let [shape1 (sphere)
          shape2 (cube)
          intersections [(intersection 1 shape1)
                         (intersection 2 shape2)
                         (intersection 3 shape1)
                         (intersection 4 shape2)]]
      (are [operation x0 x1]
        (let [csg (csg operation shape1 shape2)
              result (filter-intersections csg intersections)]
          (is (roughly (get intersections x0) (first result)))
          (is (roughly (get intersections x1) (second result))))
        :union 0 3
        :intersection 1 2
        :difference 0 1)))

  (testing "should have no intersection with ray that misses"
    (let [csg (csg :union (sphere) (cube))
          ray (ray (point 0 2 -5) (vektor 0 0 1))]
      (is (empty? (local-intersect csg ray)))))

  (testing "should be able to calculate intersection"
    (let [shape1 (sphere)
          shape2 (sphere :transform (translation 0 0 0.5))
          csg (csg :union shape1 shape2)
          ray (ray (point 0 0 -5) (vektor 0 0 1))]
      (is (roughly [(intersection 4 shape1) (intersection 6.5 shape2)] (local-intersect csg ray))))))
