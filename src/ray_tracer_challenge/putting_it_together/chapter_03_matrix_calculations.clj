(ns ray-tracer-challenge.putting-it-together.chapter-03-matrix-calculations
  (:require [ray-tracer-challenge.logic.matrices :refer :all]))

(defn -main []
  (do
    (println "=== Invert identity matrix ===")
    (println-matrix (inverse identity-matrix)))
  (let [matrix (matrix [[6 4 4 4]
                        [5 5 7 6]
                        [4 -9 3 -7]
                        [9 1 7 -6]])]
    (println "=== Multiply matrix by its inverse ===")
    (println-matrix (multiply-matrices matrix (inverse matrix))))
  (let [matrix (matrix [[6 4 4 4]
                        [5 5 7 6]
                        [4 -9 3 -7]
                        [9 1 7 -6]])]
    (println "=== Inverse of transpose of matrix")
    (println-matrix (inverse (transpose matrix)))
    (println "=== Transpose of inverse of matrix")
    (println-matrix (transpose (inverse matrix))))
  (let [matrix (matrix [[1 0 0 0]
                        [0 5 0 0]
                        [0 0 1 0]
                        [0 0 0 1]])
        tuple [1 2 3 4]]
    (println "=== Multiply changed identity matrix by tuple")
    (println (multiply-matrix-by-tuple matrix tuple))))
