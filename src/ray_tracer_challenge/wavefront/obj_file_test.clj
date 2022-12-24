(ns ray-tracer-challenge.wavefront.obj-file-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [ray-tracer-challenge.logic.triangles :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]
            [ray-tracer-challenge.wavefront.obj-file :refer :all]))

(deftest about-obj-file-parser

  (testing "should ignore unrecognized lines"
    (let [gibberish (str/join "\n" ["There was a young lady named Bright"
                                    "who traveled much faster than light."
                                    "She set out one day"
                                    "in a relative way,"
                                    "and came back the previous night."])
          parsed-file (parse-obj-file gibberish)]
      (is (= 5 (count (:ignored-lines parsed-file))))))

  (testing "should process vertex data"
    (let [file (str/join "\n" ["v -1 1 0"
                               "v -1.0000 0.5000 0.0000"
                               "v 1 0 0"
                               "v 1 1 0"])
          parsed-file (parse-obj-file file)]
      (is (roughly (point -1 1 0) (get-vertex parsed-file 1)))
      (is (roughly (point -1 0.5 0) (get-vertex parsed-file 2)))
      (is (roughly (point 1 0 0) (get-vertex parsed-file 3)))
      (is (roughly (point 1 1 0) (get-vertex parsed-file 4)))))

  (testing "should process triangle data"
    (let [file (str/join "\n" ["v -1 1 0"
                               "v -1 0 0"
                               "v 1 0 0"
                               "v 1 1 0"
                               ""
                               "f 1 2 3"
                               "f 1 3 4"])
          parsed-file (parse-obj-file file)
          [triangle1 triangle2] (:default-group parsed-file)]
      (is (roughly (get-vertex parsed-file 1) (:p1 triangle1)))
      (is (roughly (get-vertex parsed-file 2) (:p2 triangle1)))
      (is (roughly (get-vertex parsed-file 3) (:p3 triangle1)))
      (is (roughly (get-vertex parsed-file 1) (:p1 triangle2)))
      (is (roughly (get-vertex parsed-file 3) (:p2 triangle2)))
      (is (roughly (get-vertex parsed-file 4) (:p3 triangle2)))))

  (testing "should process triangulate polygonal data"
    (let [file (str/join "\n" ["v -1 1 0"
                               "v -1 0 0"
                               "v 1 0 0"
                               "v 1 1 0"
                               "v 0 2 0"
                               ""
                               "f 1 2 3 4 5"])
          parsed-file (parse-obj-file file)
          [triangle1 triangle2 triangle3] (:default-group parsed-file)]
      (is (roughly (get-vertex parsed-file 1) (:p1 triangle1)))
      (is (roughly (get-vertex parsed-file 2) (:p2 triangle1)))
      (is (roughly (get-vertex parsed-file 3) (:p3 triangle1)))
      (is (roughly (get-vertex parsed-file 1) (:p1 triangle2)))
      (is (roughly (get-vertex parsed-file 3) (:p2 triangle2)))
      (is (roughly (get-vertex parsed-file 4) (:p3 triangle2)))
      (is (roughly (get-vertex parsed-file 1) (:p1 triangle3)))
      (is (roughly (get-vertex parsed-file 4) (:p2 triangle3)))
      (is (roughly (get-vertex parsed-file 5) (:p3 triangle3)))))

  (testing "should support named groups"
    (let [file (str/join "\n" ["v -1 1 0"
                               "v -1 0 0"
                               "v 1 0 0"
                               "v 1 1 0"
                               ""
                               "g FirstGroup"
                               "f 1 2 3"
                               "f 2 3 4"
                               "g SecondGroup"
                               "f 1 3 4"])
          parsed-file (parse-obj-file file)
          [triangle1 triangle2] (get-group parsed-file "FirstGroup")
          [triangle3] (get-group parsed-file "SecondGroup")]
      (is (roughly (get-vertex parsed-file 1) (:p1 triangle1)))
      (is (roughly (get-vertex parsed-file 2) (:p2 triangle1)))
      (is (roughly (get-vertex parsed-file 3) (:p3 triangle1)))
      (is (roughly (get-vertex parsed-file 2) (:p1 triangle2)))
      (is (roughly (get-vertex parsed-file 3) (:p2 triangle2)))
      (is (roughly (get-vertex parsed-file 4) (:p3 triangle2)))
      (is (roughly (get-vertex parsed-file 1) (:p1 triangle3)))
      (is (roughly (get-vertex parsed-file 3) (:p2 triangle3)))
      (is (roughly (get-vertex parsed-file 4) (:p3 triangle3))))))

(deftest about-obj-to-group-converter

  (testing "should be able to convert parsed OBJ to a group"
    (let [file (str/join "\n" ["v -1 1 0"
                               "v -1 0 0"
                               "v 1 0 0"
                               "v 1 1 0"
                               ""
                               "f 3 4 1"
                               "g FirstGroup"
                               "f 1 2 3"
                               "f 2 3 4"
                               "g SecondGroup"
                               "f 1 3 4"])
          group (obj-to-group (parse-obj-file file))
          triangles-to-points (fn [triangles] (map #(vector (:p1 %) (:p2 %) (:p3 %)) triangles))]
      (is (= (set [[[(point 1.0 0.0 0.0) (point 1.0 1.0 0.0) (point -1.0 1.0 0.0)]]
                   [[(point -1.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0)]
                    [(point -1.0 0.0 0.0) (point 1.0 0.0 0.0) (point 1.0 1.0 0.0)]]
                   [[(point -1.0 1.0 0.0) (point 1.0 0.0 0.0) (point 1.0 1.0 0.0)]]])
             (set [(triangles-to-points (get-in group [:children 0 :children]))
                   (triangles-to-points (get-in group [:children 1 :children]))
                   (triangles-to-points (get-in group [:children 2 :children]))]))))))
