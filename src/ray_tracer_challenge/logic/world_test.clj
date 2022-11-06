(ns ray-tracer-challenge.logic.world-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
            [ray-tracer-challenge.logic.lights :refer :all]
            [ray-tracer-challenge.logic.materials :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.spheres :refer :all]
            [ray-tracer-challenge.logic.transformations :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.logic.world :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(defn- default-world [& {:keys [light] :or {light (point-light (point -10 10 -10) (color 1 1 1))}}]
  (let [s1 (sphere :material (material :color (color 0.8 1.0 0.6) :diffuse 0.7 :specular 0.2))
        s2 (sphere :transform (scaling 0.5 0.5 0.5))]
    (world :light light :objects [s1 s2])))

(deftest about-world

  (testing "should be able to create empty world"
    (let [world (world)]
      (is (nil? (:light world)))
      (is (empty? (:objects world)))))

  (testing "should be able to set individual components"
    (let [light (point-light (point -10 10 -10) (color 1 1 1))
          s1 (sphere)
          s2 (sphere)
          world (world :light light :objects [s1 s2])]
      (is (= light (:light world)))
      (is (contains? (set (:objects world)) s1))
      (is (contains? (set (:objects world)) s2))))

  (testing "should be abe to intersect with a ray"
    (let [world (default-world)
          ray (ray (point 0 0 -5) (vektor 0 0 1))
          xs (intersect-world world ray)]
      (is (roughly [4 4.5 5.5 6] (mapv :t xs)))))

  (testing "should be able to shade an intersection"
    (let [world (default-world)
          ray (ray (point 0 0 -5) (vektor 0 0 1))
          shape (first (:objects world))
          intersection (intersection 4 shape)
          comps (prepare-computation intersection ray)]
      (is (roughly (color 0.38066 0.47583 0.2855) (shade-hit world comps)))))

  (testing "should be able to shade an intersection from the inside"
    (let [world (default-world :light (point-light (point 0 0.25 0) (color 1 1 1)))
          ray (ray (point 0 0 0) (vektor 0 0 1))
          shape (second (:objects world))
          intersection (intersection 0.5 shape)
          comps (prepare-computation intersection ray)]
      (is (roughly (color 0.90498 0.90498 0.90498) (shade-hit world comps)))))

  (testing "should be able to determine color when ray misses"
    (let [world (default-world)
          ray (ray (point 0 0 -5) (vektor 0 1 0))]
      (is (roughly black (color-at world ray)))))

  (testing "should be able to determine color when ray hits"
    (let [world (default-world)
          ray (ray (point 0 0 -5) (vektor 0 0 1))]
      (is (roughly (color 0.38066 0.47583 0.2855) (color-at world ray)))))

  (testing "should be able to determine color when intersection is behind ray"
    (let [world (-> (default-world)
                    (assoc-in [:objects 0 :material :ambient] 1)
                    (assoc-in [:objects 1 :material :ambient] 1))
          inner (second (:objects world))
          ray (ray (point 0 0 0.75) (vektor 0 0 -1))]
      (is (roughly (get-in inner [:material :color]) (color-at world ray))))))
