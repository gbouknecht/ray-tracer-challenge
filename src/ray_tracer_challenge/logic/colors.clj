(ns ray-tracer-challenge.logic.colors)

(defn color [r g b] [r g b])
(defn add-colors
  ([[r1 g1 b1] [r2 g2 b2]] [(+ r1 r2) (+ g1 g2) (+ b1 b2)])
  ([color1 color2 & colors] (reduce add-colors (concat [color1 color2] colors))))
(defn subtract-colors [[r1 g1 b1] [r2 g2 b2]] [(- r1 r2) (- g1 g2) (- b1 b2)])
(defn multiply-color [[r g b] factor] [(* r factor) (* g factor) (* b factor)])
(defn multiply-colors [[r1 g1 b1] [r2 g2 b2]] [(* r1 r2) (* g1 g2) (* b1 b2)])

(def black (color 0 0 0))
(def red (color 1 0 0))
(def green (color 0 1 0))
(def blue (color 0 0 1))
(def white (color 1 1 1))
