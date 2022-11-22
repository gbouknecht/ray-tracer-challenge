(ns ray-tracer-challenge.logic.planes
  (:require [ray-tracer-challenge.logic.constants :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
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
          (local-normal-at [_ _] (vektor 0 1 0))]
    (shape local-intersect local-normal-at transform material)))
