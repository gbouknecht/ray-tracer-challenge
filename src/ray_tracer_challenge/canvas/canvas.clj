(ns ray-tracer-challenge.canvas.canvas
  (:require [clojure.math.numeric-tower :refer [round]]
            [clojure.string :as str]))

(defn canvas [width height] {:width width :height height :pixels (vec (repeat (* width height) [0 0 0]))})
(defn- index-of-pixel-at [canvas x y] (+ x (* y (:width canvas))))
(defn pixel-at [canvas x y] ((:pixels canvas) (index-of-pixel-at canvas x y)))
(defn write-pixel [canvas x y color]
  (if (and (<= 0 x (dec (:width canvas)))
           (<= 0 y (dec (:height canvas))))
    (assoc canvas :pixels (assoc (:pixels canvas) (index-of-pixel-at canvas x y) color))
    canvas))

(defn- split-at-70-chars [s]
  (let [max-line-length 70]
    (loop [result [] s s]
      (if (empty? s)
        (str/join "\n" result)
        (let [length (.length s)
              index (if (<= length max-line-length) length (str/last-index-of s " " max-line-length))
              first (subs s 0 index)
              rest (str/trim (subs s index))]
          (recur (conj result first) rest))))))

(defn canvas-to-ppm [canvas]
  (let [max-color-value 255
        header ["P3"
                (str (:width canvas) " " (:height canvas))
                max-color-value]
        pixel-data (->> (:pixels canvas)
                        (flatten)
                        (map #(->> % (* max-color-value) round (max 0) (min max-color-value)))
                        (partition (* 3 (:width canvas)))
                        (map #(->> % (str/join " ") split-at-70-chars)))]
    (str (str/join "\n" (concat header pixel-data)) "\n")))
