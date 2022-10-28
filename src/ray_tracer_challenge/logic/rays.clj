(ns ray-tracer-challenge.logic.rays
  (:require [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [clojure.math.numeric-tower :refer [sqrt]]))

(defn ray [origin direction] {:origin origin :direction direction})
(defn position [ray t] (add-tuples (:origin ray) (multiply-tuple (:direction ray) t)))
(defn transform [ray matrix]
  {:origin    (multiply-matrix-by-tuple matrix (:origin ray))
   :direction (multiply-matrix-by-tuple matrix (:direction ray))})
(defn sphere [] {:uuid (random-uuid) :transform identity-matrix})
(defn set-transform [sphere transform] (assoc sphere :transform transform))
(defn intersection [t object] {:t t :object object})
(defn intersection? [x] (and (map? x) (= (set (keys x)) #{:t :object})))
(defn intersect [sphere ray]
  (let [ray (transform ray (inverse (:transform sphere)))
        sphere-to-ray (subtract-tuples (:origin ray) (point 0 0 0))
        a (dot-product-tuples (:direction ray) (:direction ray))
        b (* (dot-product-tuples (:direction ray) sphere-to-ray) 2)
        c (dec (dot-product-tuples sphere-to-ray sphere-to-ray))
        discriminant (- (* b b) (* 4 a c))]
    (if (< discriminant 0)
      []
      (let [t1 (/ (- (- b) (sqrt discriminant)) (* 2 a))
            t2 (/ (+ (- b) (sqrt discriminant)) (* 2 a))]
        [(intersection t1 sphere) (intersection t2 sphere)]))))
(defn hit [intersections]
  (when-let [positive-intersections (seq (filter #(pos? (:t %)) intersections))]
    (apply min-key :t positive-intersections)))
