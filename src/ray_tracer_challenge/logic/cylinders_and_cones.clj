(ns ray-tracer-challenge.logic.cylinders-and-cones
  (:require [clojure.math.numeric-tower :refer [sqrt]]
            [ray-tracer-challenge.logic.constants :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn- close-to-zero? [value] (< (abs value) epsilon))
(defn- intersect-caps [shape local-ray]
  (let [[origin-x origin-y origin-z] (:origin local-ray)
        [direction-x direction-y direction-z] (:direction local-ray)
        check-cap (fn [t]
                    (let [x (+ origin-x (* t direction-x))
                          y (+ origin-y (* t direction-y))
                          z (+ origin-z (* t direction-z))
                          radius (condp = (:type shape) :cylinder 1 :cone y)]
                      (<= (+ (* x x) (* z z)) (* radius radius))))]
    (if (or (not (:closed shape)) (close-to-zero? direction-y))
      []
      (let [t0 (/ (- (:minimum shape) origin-y) direction-y)
            t1 (/ (- (:maximum shape) origin-y) direction-y)]
        (->> [t0 t1]
             (filter check-cap)
             (mapv #(intersection % shape)))))))
(defn- make-cylinder-or-cone [type transform material minimum maximum closed]
  (let [minimum (or minimum ##-Inf)
        maximum (or maximum ##Inf)
        closed (or closed false)
        local-intersect (fn [shape local-ray]
                          (let [[origin-x origin-y origin-z] (:origin local-ray)
                                [direction-x direction-y direction-z] (:direction local-ray)
                                [a b c] (condp = (:type shape)
                                          :cylinder [(+ (* direction-x direction-x) (* direction-z direction-z))
                                                     (+ (* 2 origin-x direction-x) (* 2 origin-z direction-z))
                                                     (+ (* origin-x origin-x) (* origin-z origin-z) -1)]
                                          :cone [(+ (- (* direction-x direction-x) (* direction-y direction-y)) (* direction-z direction-z))
                                                 (+ (- (* 2 origin-x direction-x) (* 2 origin-y direction-y)) (* 2 origin-z direction-z))
                                                 (+ (- (* origin-x origin-x) (* origin-y origin-y)) (* origin-z origin-z))])
                                intersections (cond
                                                (and (close-to-zero? a) (close-to-zero? b)) []
                                                (close-to-zero? a) [(intersection (/ (- c) (* 2 b)) shape)]
                                                :else (let [disc (- (* b b) (* 4 a c))]
                                                        (if (< disc 0)
                                                          []
                                                          (let [[t0 t1] (sort [(/ (- (- b) (sqrt disc)) (* 2 a))
                                                                               (/ (+ (- b) (sqrt disc)) (* 2 a))])
                                                                y0 (+ origin-y (* t0 direction-y))
                                                                y1 (+ origin-y (* t1 direction-y))
                                                                within-range? (fn [[y _]] (< (:minimum shape) y (:maximum shape)))
                                                                make-intersection (fn [[_ t]] (intersection t shape))]
                                                            (->> [[y0 t0] [y1 t1]]
                                                                 (filter within-range?)
                                                                 (mapv make-intersection))))))]
                            (into intersections (intersect-caps shape local-ray))))
        local-normal-at (fn [shape local-point]
                          (let [[x y z] local-point
                                dist (+ (* x x) (* z z))
                                radius (condp = (:type shape) :cylinder 1 :cone y)]
                            (cond
                              (and (< dist (* radius radius)) (>= y (- (:maximum shape) epsilon))) (vektor 0 1 0)
                              (and (< dist (* radius radius)) (<= y (+ (:minimum shape) epsilon))) (vektor 0 -1 0)
                              :else (let [y (condp = (:type shape)
                                              :cylinder 0
                                              :cone (* (sqrt dist) (if (pos? y) -1 1)))]
                                      (vektor x y z)))))]
    (-> (shape type transform material local-intersect local-normal-at)
        (assoc :minimum minimum :maximum maximum :closed closed))))
(defn cylinder [& {:keys [transform material minimum maximum closed]}]
  (make-cylinder-or-cone :cylinder transform material minimum maximum closed))
(defn cone [& {:keys [transform material minimum maximum closed]}]
  (make-cylinder-or-cone :cone transform material minimum maximum closed))
