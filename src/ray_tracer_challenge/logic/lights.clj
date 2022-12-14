(ns ray-tracer-challenge.logic.lights
  (:require [clojure.math.numeric-tower :refer [expt]]
            [ray-tracer-challenge.logic.colors :refer :all]
            [ray-tracer-challenge.logic.patterns :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn point-light [position intensity] {:position position :intensity intensity})
(defn lighting [material object light point eye-vektor normal-vektor in-shadow]
  (let [color (if-let [pattern (:pattern material)] (pattern-at-shape pattern object point) (:color material))
        effective-color (multiply-colors color (:intensity light))
        light-vektor (normalize-vektor (subtract-tuples (:position light) point))
        ambient-color (multiply-color effective-color (:ambient material))
        light-dot-normal (dot-product-tuples light-vektor normal-vektor)
        diffuse-color (if (or (< light-dot-normal 0) in-shadow)
                        black
                        (reduce multiply-color [effective-color (:diffuse material) light-dot-normal]))
        specular-color (if (or (< light-dot-normal 0) in-shadow)
                         black
                         (let [reflect-vektor (reflect (negate-tuple light-vektor) normal-vektor)
                               reflect-dot-eye (dot-product-tuples reflect-vektor eye-vektor)]
                           (if (<= reflect-dot-eye 0)
                             black
                             (let [factor (expt reflect-dot-eye (:shininess material))]
                               (reduce multiply-color [(:intensity light) (:specular material) factor])))))]
    (reduce add-colors [ambient-color diffuse-color specular-color])))
