(ns ray-tracer-challenge.logic.triangles
  (:require [ray-tracer-challenge.logic.common :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn- make-triangle [type p1 p2 p3 n1 n2 n3 transform material]
  (letfn [(local-intersect [triangle local-ray]
            (let [direction-cross-e2 (cross-product-vektors (:direction local-ray) (:e2 triangle))
                  determinant (dot-product-tuples (:e1 triangle) direction-cross-e2)]
              (if (close-to-zero? determinant)
                []
                (let [f (/ 1.0 determinant)
                      p1-to-origin (subtract-tuples (:origin local-ray) (:p1 triangle))
                      u (* f (dot-product-tuples p1-to-origin direction-cross-e2))]
                  (if (or (< u 0) (> u 1))
                    []
                    (let [origin-cross-e1 (cross-product-vektors p1-to-origin (:e1 triangle))
                          v (* f (dot-product-tuples (:direction local-ray) origin-cross-e1))]
                      (if (or (< v 0) (> (+ u v) 1))
                        []
                        (let [t (* f (dot-product-tuples (:e2 triangle) origin-cross-e1))]
                          [(intersection t triangle u v)]))))))))
          (local-normal-at [shape _ hit]
            (condp = (:type shape)
              :triangle (:normal shape)
              :smooth-triangle (let [u (:u hit)
                                     v (:v hit)]
                                 (reduce add-tuples [(multiply-tuple (:n2 shape) u)
                                                     (multiply-tuple (:n3 shape) v)
                                                     (multiply-tuple (:n1 shape) (- 1 u v))]))))]
    (let [e1 (subtract-tuples p2 p1)
          e2 (subtract-tuples p3 p1)
          normal (normalize-vektor (cross-product-vektors e2 e1))]
      (-> (shape type transform material local-intersect local-normal-at)
          (assoc :p1 p1 :p2 p2 :p3 p3
                 :n1 n1 :n2 n2 :n3 n3
                 :e1 e1 :e2 e2 :normal normal)))))
(defn triangle [p1 p2 p3 & {:keys [transform material]}]
  (make-triangle :triangle p1 p2 p3 nil nil nil transform material))
(defn smooth-triangle [p1 p2 p3 n1 n2 n3 & {:keys [transform material]}]
  (make-triangle :smooth-triangle p1 p2 p3 n1 n2 n3 transform material))
