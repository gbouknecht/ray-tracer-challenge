(ns ray-tracer-challenge.logic.materials
  (:require [ray-tracer-challenge.logic.colors :refer :all]))

(defn material [& {:keys [pattern color ambient diffuse specular shininess reflective]
                   :or   {pattern    nil
                          color      white
                          ambient    0.1
                          diffuse    0.9
                          specular   0.9
                          shininess  200.0
                          reflective 0.0}}]
  {:pattern    pattern
   :color      color
   :ambient    ambient
   :diffuse    diffuse
   :specular   specular
   :shininess  shininess
   :reflective reflective})
