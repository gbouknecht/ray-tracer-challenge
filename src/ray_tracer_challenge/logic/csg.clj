(ns ray-tracer-challenge.logic.csg
  (:require [ray-tracer-challenge.logic.shapes :refer :all]))

(declare filter-intersections)
(defn csg [operation left right & {:keys [transform]}]
  (letfn [(local-intersect [csg local-ray] (->> (:children csg)
                                                (mapcat #(intersect % local-ray))
                                                (sort-by :t)
                                                (filter-intersections csg)))
          (local-normal-at [_ _ _] (throw (UnsupportedOperationException. "local-normal-at not supported for csg")))]
    (-> (shape :csg transform nil local-intersect local-normal-at)
        (assoc :operation operation :left left :right right :children [left right]))))
(defn intersection-allowed? [operation left-hit inside-left inside-right]
  (condp = operation
    :union (or (and left-hit (not inside-right)) (and (not left-hit) (not inside-left)))
    :intersection (or (and left-hit inside-right) (and (not left-hit) inside-left))
    :difference (or (and left-hit (not inside-right)) (and (not left-hit) inside-left))))
(defn filter-intersections [csg intersections]
  (let [operation (:operation csg)
        all-left-shapes (tree-seq :children :children (:left csg))
        result (reduce (fn [result intersection]
                         (let [left-hit (some #{(:object intersection)} all-left-shapes)
                               inside-left (:inside-left result)
                               inside-right (:inside-right result)
                               allowed (intersection-allowed? operation left-hit inside-left inside-right)]
                           (cond-> result
                                   allowed (update :intersections conj intersection)
                                   left-hit (update :inside-left not)
                                   (not left-hit) (update :inside-right not))))
                       {:inside-left   false
                        :inside-right  false
                        :intersections []}
                       intersections)]
    (:intersections result)))
