(ns ray-tracer-challenge.logic.cubes
  (:require [ray-tracer-challenge.logic.constants :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn- check-axis [origin direction]
  (let [t-min-numerator (- -1 origin)
        t-max-numerator (- 1 origin)
        [t-min t-max] (if (>= (abs direction) epsilon)
                      [(/ t-min-numerator direction) (/ t-max-numerator direction)]
                      [(* t-min-numerator ##Inf) (* t-max-numerator ##Inf)])]
    (sort [t-min t-max])))

(defn cube [& {:keys [transform material]}]
  (letfn [(local-intersect [cube local-ray]
            (let [[origin-x origin-y origin-z] (:origin local-ray)
                  [direction-x direction-y direction-z] (:direction local-ray)
                  [xt-min xt-max] (check-axis origin-x direction-x)
                  [yt-min yt-max] (check-axis origin-y direction-y)
                  [zt-min zt-max] (check-axis origin-z direction-z)
                  t-min (max xt-min yt-min zt-min)
                  t-max (min xt-max yt-max zt-max)]
              (if (> t-min t-max) [] [(intersection t-min cube) (intersection t-max cube)])))
          (local-normal-at [_ local-point]
            (let [[x y z] local-point]
              (condp = (max (abs x) (abs y) (abs z))
                (abs x) (vektor x 0 0)
                (abs y) (vektor 0 y 0)
                (abs z) (vektor 0 0 z))))]
    (shape transform material local-intersect local-normal-at)))
