(ns ray-tracer-challenge.logic.materials
  (:require [ray-tracer-challenge.logic.colors :refer :all]))

(defn material [& {:keys [color ambient diffuse specular shininess]
                   :or   {color     (color 1 1 1)
                          ambient   0.1
                          diffuse   0.9
                          specular  0.9
                          shininess 200.0}}]
  {:color     color
   :ambient   ambient
   :diffuse   diffuse
   :specular  specular
   :shininess shininess})
