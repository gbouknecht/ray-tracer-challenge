(ns ray-tracer-challenge.logic.planes
  (:require [ray-tracer-challenge.logic.common :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn plane [& {:keys [transform material]}]
  (letfn [(local-intersect [plane local-ray]
            (let [[_ origin-y _] (:origin local-ray)
                  [_ direction-y _] (:direction local-ray)]
              (if (< (abs direction-y) epsilon)
                []
                (let [t (/ (- origin-y) direction-y)]
                  [(intersection t plane)]))))
          (local-normal-at [_ _ _] (vektor 0 1 0))]
    (shape :plane transform material local-intersect local-normal-at)))
