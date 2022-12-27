(ns ray-tracer-challenge.logic.intersections
  (:require [clojure.math.numeric-tower :refer [expt sqrt]]
            [ray-tracer-challenge.logic.common :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn intersection
  ([t object] (intersection t object nil nil))
  ([t object u v] {:t t :object object :u u :v v}))
(defn intersection? [x] (and (map? x) (= (set (keys x)) #{:t :object :u :v})))
(defn hit [intersections]
  (when-let [positive-intersections (seq (filter #(pos? (:t %)) intersections))]
    (apply min-key :t positive-intersections)))
(defn- find-n1-n2 [hit intersections]
  (loop [intersections intersections
         containing-objects []]
    (let [{intersected-object :object :as intersection} (first intersections)
          from-object (last containing-objects)
          containing-objects (if (some #{intersected-object} containing-objects)
                               (remove #{intersected-object} containing-objects)
                               (conj containing-objects intersected-object))
          to-object (last containing-objects)]
      (if (= hit intersection)
        (map #(get-in % [:material :refractive-index] 1.0) [from-object to-object])
        (recur (rest intersections) containing-objects)))))
(defn prepare-computation
  ([intersection ray shape-to-parent]
   (prepare-computation intersection ray [intersection] shape-to-parent))
  ([intersection ray intersections shape-to-parent]
   (let [t (:t intersection)
         object (:object intersection)
         point (position ray t)
         eye-vektor (negate-tuple (:direction ray))
         [normal-vektor inside] (let [normal-vektor (normal-at object point intersection shape-to-parent)
                                      inside (neg? (dot-product-tuples normal-vektor eye-vektor))]
                                  [(if inside (negate-tuple normal-vektor) normal-vektor) inside])
         [n1 n2] (find-n1-n2 intersection intersections)]
     {:t               t
      :object          object
      :point           point
      :eye-vektor      eye-vektor
      :inside          inside
      :normal-vektor   normal-vektor
      :over-point      (add-tuples point (multiply-tuple normal-vektor epsilon))
      :under-point     (subtract-tuples point (multiply-tuple normal-vektor epsilon))
      :reflect-vektor  (reflect (:direction ray) normal-vektor)
      :n1              n1
      :n2              n2
      :shape-to-parent shape-to-parent})))
(defn schlick [comps]
  (let [n1 (:n1 comps)
        n2 (:n2 comps)
        [result cos] (let [cos-i (dot-product-tuples (:eye-vektor comps) (:normal-vektor comps))]
                       (if (> n1 n2)
                         (let [n (/ n1 n2)
                               sin2-t (* (expt n 2) (- 1.0 (expt cos-i 2)))]
                           (if (> sin2-t 1.0)
                             [1.0 nil]
                             (let [cos-t (sqrt (- 1.0 sin2-t))] [nil cos-t])))
                         [nil cos-i]))]
    (if-not (nil? result)
      result
      (let [r0 (expt (/ (- n1 n2) (+ n1 n2)) 2)]
        (+ r0 (* (- 1 r0) (expt (- 1 cos) 5)))))))
