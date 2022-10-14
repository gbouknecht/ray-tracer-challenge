(ns ray-tracer-challenge.logic.colors)

(defn add-colors [[r1 g1 b1] [r2 g2 b2]] [(+ r1 r2) (+ g1 g2) (+ b1 b2)])
(defn subtract-colors [[r1 g1 b1] [r2 g2 b2]] [(- r1 r2) (- g1 g2) (- b1 b2)])
(defn multiply-color [[r g b] factor] [(* r factor) (* g factor) (* b factor)])
(defn multiply-colors [[r1 g1 b1] [r2 g2 b2]] [(* r1 r2) (* g1 g2) (* b1 b2)])
