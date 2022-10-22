(ns ray-tracer-challenge.logic.matrices-test
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.test.test-utils :refer :all]))

(deftest about-matrices

  (testing
    "should be able to create 4x4 matrix"
    (let [matrix (matrix [[1 2 3 4]
                          [5.5 6.5 7.5 8.5]
                          [9 10 11 12]
                          [13.5 14.5 15.5 16.5]])]
      (is (roughly (value-at matrix 0 0) 1))
      (is (roughly (value-at matrix 0 3) 4))
      (is (roughly (value-at matrix 1 0) 5.5))
      (is (roughly (value-at matrix 1 2) 7.5))
      (is (roughly (value-at matrix 2 2) 11))
      (is (roughly (value-at matrix 3 0) 13.5))
      (is (roughly (value-at matrix 3 2) 15.5))))

  (testing
    "should be able to create 2x2 matrix"
    (let [matrix (matrix [[-3 5]
                          [1 -2]])]
      (is (roughly (value-at matrix 0 0) -3))
      (is (roughly (value-at matrix 0 1) 5))
      (is (roughly (value-at matrix 1 0) 1))
      (is (roughly (value-at matrix 1 1) -2))))

  (testing
    "should be able to create 3x3 matrix"
    (let [matrix (matrix [[-3 5 0]
                          [1 -2 -7]
                          [0 1 1]])]
      (is (roughly (value-at matrix 0 0) -3))
      (is (roughly (value-at matrix 1 1) -2))
      (is (roughly (value-at matrix 2 2) 1))))

  (testing
    "should be able to check if something is a matrix"
    (let [matrix (matrix [[1 2 3] [4 5 6]])]
      (is (matrix? matrix))
      (is (not (matrix? (dissoc matrix :row-count))))
      (is (not (matrix? (dissoc matrix :col-count))))
      (is (not (matrix? (dissoc matrix :values)))))
    (is (not (matrix? [[1 2 3] [4 5 6]])))
    (is (not (matrix? [1 2 3 4 5 6])))
    (is (not (matrix? 1)))))

(deftest about-matrix-operations

  (testing
    "should be able to multiply two matrices"
    (is (roughly (multiply-matrices (matrix [[1 2 3 4]
                                             [5 6 7 8]
                                             [9 8 7 6]
                                             [5 4 3 2]])
                                    (matrix [[-2 1 2 3]
                                             [3 2 1 -1]
                                             [4 3 6 5]
                                             [1 2 7 8]]))
                 (matrix [[20 22 50 48]
                          [44 54 114 108]
                          [40 58 110 102]
                          [16 26 46 42]]))))

  (testing
    "should be able to multiply matrix by a tuple"
    (is (roughly (multiply-matrix-by-tuple (matrix [[1 2 3 4]
                                                    [2 4 4 2]
                                                    [8 6 4 1]
                                                    [0 0 0 1]])
                                           [1 2 3 1])
                 [18 24 33 1])))

  (testing
    "should get same matrix when multiplied by identity matrix"
    (let [matrix (matrix [[0 1 2 4]
                          [1 2 4 8]
                          [2 4 8 16]
                          [4 8 16 32]])]
      (is (roughly (multiply-matrices matrix identity-matrix) matrix))))

  (testing
    "should get same tuple when identity matrix is multiplied by tuple"
    (let [tuple [1 2 3 4]]
      (is (roughly (multiply-matrix-by-tuple identity-matrix tuple) tuple))))

  (testing
    "should be able to transpose matrix"
    (is (roughly (transpose (matrix [[0 9 3 0]
                                     [9 8 0 8]
                                     [1 8 5 3]
                                     [0 0 5 8]]))
                 (matrix [[0 9 1 0]
                          [9 8 8 0]
                          [3 0 5 5]
                          [0 8 3 8]]))))

  (testing
    "should get identity matrix when transposing identity matrix"
    (is (roughly (transpose identity-matrix) identity-matrix)))

  (testing
    "should be able to calculate determinant of 2x2 matrix"
    (is (roughly (determinant (matrix [[1 5] [-3 2]])) 17)))

  (testing
    "should be able to get submatrix"
    (is (roughly (submatrix (matrix [[1 5 0]
                                     [-3 2 7]
                                     [0 6 -3]]) 0 2)
                 (matrix [[-3 2]
                          [0 6]])))
    (is (roughly (submatrix (matrix [[-6 1 1 6]
                                     [-8 5 8 6]
                                     [-1 0 8 2]
                                     [-7 1 -1 1]]) 2 1)
                 (matrix [[-6 1 6]
                          [-8 8 6]
                          [-7 -1 1]]))))

  (testing
    "should be able to calculate minor of 3x3 matrix"
    (let [matrix (matrix [[3 5 0]
                          [2 -1 -7]
                          [6 -1 5]])
          submatrix (submatrix matrix 1 0)]
      (is (roughly (determinant submatrix) 25))
      (is (roughly (minor matrix 1 0) 25))))

  (testing
    "should be able to calculate cofactor of 3x3 matrix"
    (let [matrix (matrix [[3 5 0]
                          [2 -1 -7]
                          [6 -1 5]])]
      (is (roughly (minor matrix 0 0) -12))
      (is (roughly (cofactor matrix 0 0) -12))
      (is (roughly (minor matrix 1 0) 25))
      (is (roughly (cofactor matrix 1 0) -25))))

  (testing
    "should be able to calculate determinant of 3x3 matrix"
    (let [matrix (matrix [[1 2 6]
                          [-5 8 -4]
                          [2 6 4]])]
      (is (roughly (cofactor matrix 0 0) 56))
      (is (roughly (cofactor matrix 0 1) 12))
      (is (roughly (cofactor matrix 0 2) -46))
      (is (roughly (determinant matrix) -196))))

  (testing
    "should be able to calculate determinant of 4x4 matrix"
    (let [matrix (matrix [[-2 -8 3 5]
                          [-3 1 7 3]
                          [1 2 -9 6]
                          [-6 7 7 -9]])]
      (is (roughly (cofactor matrix 0 0) 690))
      (is (roughly (cofactor matrix 0 1) 447))
      (is (roughly (cofactor matrix 0 2) 210))
      (is (roughly (cofactor matrix 0 3) 51))
      (is (roughly (determinant matrix) -4071))))

  (testing
    "should be able to test invertible matrix for invertibility"
    (let [matrix (matrix [[6 4 4 4]
                          [5 5 7 6]
                          [4 -9 3 -7]
                          [9 1 7 -6]])]
      (is (roughly (determinant matrix) -2120))
      (is (invertible? matrix))))

  (testing
    "should be able to test non-invertible matrix for invertibility"
    (let [matrix (matrix [[-4 2 -2 -3]
                          [9 6 2 6]
                          [0 -5 1 -5]
                          [0 0 0 0]])]
      (is (= (determinant matrix) 0))
      (is (not (invertible? matrix)))))

  (testing
    "should be able to calculate inverse of matrix"
    (let [matrix-a (matrix [[-5 2 6 -8]
                            [1 -5 1 8]
                            [7 7 -6 -7]
                            [1 -3 7 4]])
          matrix-b (inverse matrix-a)]
      (is (roughly (determinant matrix-a) 532))
      (is (roughly (cofactor matrix-a 2 3) -160))
      (is (roughly (value-at matrix-b 3 2) (/ -160 532)))
      (is (roughly (cofactor matrix-a 3 2) 105))
      (is (roughly (value-at matrix-b 2 3) (/ 105 532)))
      (is (roughly matrix-b
                   (matrix [[0.21805 0.45113 0.24060 -0.04511]
                            [-0.80827 -1.45677 -0.44361 0.52068]
                            [-0.07895 -0.22368 -0.05263 0.19737]
                            [-0.52256 -0.81391 -0.30075 0.30639]]))))
    (is (roughly (inverse (matrix [[8 -5 9 2]
                                   [7 5 6 1]
                                   [-6 0 9 6]
                                   [-3 0 -9 -4]]))
                 (matrix [[-0.15385 -0.15385 -0.28205 -0.53846]
                          [-0.07692 0.12308 0.02564 0.03077]
                          [0.35897 0.35897 0.43590 0.92308]
                          [-0.69231 -0.69231 -0.76923 -1.92308]])))
    (is (roughly (inverse (matrix [[9 3 0 9]
                                   [-5 -2 -6 -3]
                                   [-4 9 6 4]
                                   [-7 6 6 2]]))
                 (matrix [[-0.04074 -0.07778 0.14444 -0.22222]
                          [-0.07778 0.03333 0.36667 -0.33333]
                          [-0.02901 -0.14630 -0.10926 0.12963]
                          [0.17778 0.06667 -0.26667 0.33333]])))
    (let [matrix-a (matrix [[3 -9 7 3]
                            [3 -8 2 -9]
                            [-4 4 4 1]
                            [-6 5 -1 1]])
          matrix-b (matrix [[8 2 2 2]
                            [3 -1 7 0]
                            [7 0 5 4]
                            [6 -2 0 5]])
          matrix-c (multiply-matrices matrix-a matrix-b)]
      (is (roughly (multiply-matrices matrix-c (inverse matrix-b)) matrix-a)))))
