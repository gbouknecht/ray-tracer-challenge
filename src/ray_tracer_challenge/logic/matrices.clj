(ns ray-tracer-challenge.logic.matrices
  (:require [clojure.string :as str]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn matrix [values]
  (assert (apply = (map count values)) "rows should have same length")
  {:row-count (count values) :col-count (count (first values)) :values (vec (flatten values))})
(def identity-matrix (matrix [[1 0 0 0] [0 1 0 0] [0 0 1 0] [0 0 0 1]]))
(defn- index-of-value-at [matrix row col] (+ (* row (:col-count matrix)) col))
(defn value-at [matrix row col] ((:values matrix) (index-of-value-at matrix row col)))
(defn- row-at [matrix row]
  (let [index-start (index-of-value-at matrix row 0)
        index-end (+ index-start (:col-count matrix))]
    (subvec (:values matrix) index-start index-end)))
(defn- col-at [matrix col] (mapv #(value-at matrix % col) (range 0 (:row-count matrix))))
(defn multiply-matrices [matrix1 matrix2]
  (let [row-count (:row-count matrix1)
        col-count (:col-count matrix2)]
    (assert (= (:col-count matrix1) (:row-count matrix2))
            (format "col count matrix1 (%d) should equal row count matrix2 (%d)", (:col-count matrix1) (:row-count matrix2)))
    {:row-count row-count
     :col-count col-count
     :values    (vec (for [row (range 0 row-count) col (range 0 col-count)]
                       (dot-product-tuples (row-at matrix1 row) (col-at matrix2 col))))}))
(defn multiply-matrix-by-tuple [matrix tuple]
  (assert (= (:col-count matrix) (count tuple))
          (format "col count matrix (%d) should equal tuple length (%d)" (:col-count matrix) (count tuple)))
  (mapv #(dot-product-tuples (row-at matrix %) tuple) (range 0 (:row-count matrix))))
(defn transpose [matrix]
  (let [row-count (:row-count matrix)
        col-count (:col-count matrix)]
    (assert (= row-count col-count) (format "row count (%d) should equal col count (%d)" row-count col-count))
    {:row-count row-count
     :col-count col-count
     :values    (vec (flatten (for [row (range 0 row-count)] (col-at matrix row))))}))
(declare cofactor)
(defn determinant [matrix]
  (assert (= (:row-count matrix) (:col-count matrix)) "should be nxn matrix")
  (if (= (:row-count matrix) 2)
    (let [[a b c d] (:values matrix)] (- (* a d) (* b c)))
    (apply + (map-indexed (fn [col value] (* value (cofactor matrix 0 col))) (row-at matrix 0)))))
(defn submatrix [matrix row-to-remove col-to-remove]
  (let [row-count (:row-count matrix)
        col-count (:col-count matrix)]
    {:row-count (dec row-count)
     :col-count (dec col-count)
     :values    (vec (flatten (for [row (range 0 row-count)
                                    col (range 0 col-count)
                                    :when (and (not= row row-to-remove) (not= col col-to-remove))]
                                (value-at matrix row col))))}))
(defn minor [matrix row col] (determinant (submatrix matrix row col)))
(defn cofactor [matrix row col] (let [sign (if (odd? (+ row col)) -1 1)] (* sign (minor matrix row col))))
(defn invertible? [matrix] (not= (determinant matrix) 0))
(defn inverse [matrix]
  (let [row-count (:row-count matrix)
        col-count (:col-count matrix)
        determinant (determinant matrix)]
    {:row-count row-count
     :col-count col-count
     :values    (vec (flatten (for [col (range 0 col-count) row (range 0 row-count)] (/ (cofactor matrix row col) determinant))))}))

(defn str-matrix [matrix]
  (str/join
    "\n"
    (for [row (range 0 (:row-count matrix))]
      (let [row-prefix (if (zero? row) "[[" " [")
            row-suffix (if (= row (dec (:row-count matrix))) "]]" "]")]
        (apply format (str row-prefix (str/join " " (repeat (:col-count matrix) "%11.5f")) row-suffix)
               (map double (row-at matrix row)))))))
(defn println-matrix [matrix] (println (str-matrix matrix)))
