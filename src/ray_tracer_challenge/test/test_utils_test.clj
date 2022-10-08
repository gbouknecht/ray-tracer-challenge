(ns ray-tracer-challenge.test.test-utils-test
  (:require [clojure.test :refer :all])
  (:require [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-roughly-assert-expr

  (testing
    "should be able to compare scalars"
    (are [x y] (roughly x y)
               3 3.0
               3.0 3.000009
               3.000009 3.0
               -3.0 -3.000009
               2.999991 3.0)
    (are [x y] (not-roughly x y)
               3 3.00001
               3.0 3.00001
               3.00001 3.0
               -3.0 -3.00001
               2.999990 3.0))

  (testing
    "should be able to compare vectors of scalars"
    (are [x y] (roughly x y)
               [4.3 -4.2 3.1 1.0] [4.300009 -4.199991 3.099991 0.999991]
               [] [])
    (are [x y] (not-roughly x y)
               [4.3 -4.2 3.1 1.0] [4.30001 -4.2 3.1 1.0]
               [4.3 -4.2 3.1 1.0] [4.3 -4.199990 3.1 1.0]
               [4.3 -4.2 3.1 1.0] [4.3 -4.2 3.10001 1.0]
               [4.3 -4.2 3.1 1.0] [4.3 -4.2 3.1 0.999989]
               [1 2 3 4] [1 2 3]
               [1 2 3] [1 2 3 4]))

  (testing
    "should be able to compare vectors of vectors of scalars"
    (are [x y] (roughly x y)
               [[4.3 -4.2] [3.1 1.0]] [[4.300009 -4.199991] [3.099991 0.999991]])
    (are [x y] (not-roughly x y)
               [[4.3 -4.2] [3.1 1.0]] [[4.30001 -4.2] [3.1 1.0]]
               [[4.3 -4.2] [3.1 1.0]] [[4.3 -4.199990] [3.1 1.0]]
               [[4.3 -4.2] [3.1 1.0]] [[4.3 -4.2] [3.10001 1.0]]
               [[4.3 -4.2] [3.1 1.0]] [[4.3 -4.2] [3.1 0.999989]]
               [[1 2] [3 4]] [[1 2] [3]]
               [[1 2] [3]] [[1 2] [3 4]])))
