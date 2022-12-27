(ns ray-tracer-challenge.logic.shapes
  (:require [ray-tracer-challenge.logic.materials :as materials]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn shape [type transform material local-intersect local-normal-at]
  {:uuid            (random-uuid)
   :type            type
   :transform       (or transform identity-matrix)
   :material        (or material (materials/material))
   :local-intersect local-intersect
   :local-normal-at local-normal-at})
(declare intersect)
(defn group [& {:keys [transform children]}]
  (letfn [(local-intersect [group local-ray] (->> (:children group) (mapcat #(intersect % local-ray)) (sort-by :t)))
          (local-normal-at [_ _ _] (throw (UnsupportedOperationException. "local-normal-at not support for groups")))]
    (-> (shape :group transform nil local-intersect local-normal-at)
        (assoc :children (or children [])))))
(defn shape-to-parent [shapes]
  (let [all-shapes (mapcat #(tree-seq :children :children %) shapes)
        child-parent-pairs (for [parent all-shapes child (:children parent)] [child parent])]
    (into {} (map #(update % 0 :uuid) child-parent-pairs))))
(defn- shapes-to-root [shape shape-to-parent] (take-while some? (iterate #(get shape-to-parent (:uuid %)) shape)))
(defn world-to-object [shape point shape-to-parent]
  (let [shapes (reverse (shapes-to-root shape shape-to-parent))]
    (reduce (fn [point shape] (multiply-matrix-by-tuple (inverse (:transform shape)) point)) point shapes)))
(defn normal-to-world [shape normal shape-to-parent]
  (let [shapes (shapes-to-root shape shape-to-parent)]
    (reduce (fn [normal shape]
              (-> (multiply-matrix-by-tuple (transpose (inverse (:transform shape))) normal)
                  (assoc 3 0)
                  (normalize-vektor)))
            normal shapes)))
(defn local-intersect [shape local-ray] ((:local-intersect shape) shape local-ray))
(defn intersect [shape ray]
  (let [local-ray (transform ray (inverse (:transform shape)))]
    (local-intersect shape local-ray)))
(defn local-normal-at [shape local-point hit] ((:local-normal-at shape) shape local-point hit))
(defn normal-at [shape world-point hit shape-to-parent]
  (let [local-point (world-to-object shape world-point shape-to-parent)
        local-normal (local-normal-at shape local-point hit)]
    (normal-to-world shape local-normal shape-to-parent)))
