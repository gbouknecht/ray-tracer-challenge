(ns ray-tracer-challenge.canvas.camera
  (:require [ray-tracer-challenge.canvas.canvas :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]
            [ray-tracer-challenge.logic.rays :refer :all]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]
            [ray-tracer-challenge.logic.world :refer :all]))

(defn camera [hsize vsize field-of-view & {:keys [transform] :or {transform identity-matrix}}]
  (let [half-view (Math/tan (/ field-of-view 2))
        aspect (/ hsize vsize)
        [half-width half-height] (if (>= aspect 1)
                                   [half-view (/ half-view aspect)]
                                   [(* half-view aspect) half-view])]
    {:hsize         hsize
     :vsize         vsize
     :field-of-view field-of-view
     :transform     transform
     :half-width    half-width
     :half-height   half-height
     :pixel-size    (/ (* half-width 2) hsize)}))
(defn ray-for-pixel [camera px py]
  (let [xoffset (* (+ px 0.5) (:pixel-size camera))
        yoffset (* (+ py 0.5) (:pixel-size camera))
        world-x (- (:half-width camera) xoffset)
        world-y (- (:half-height camera) yoffset)
        pixel (multiply-matrix-by-tuple (inverse (:transform camera)) (point world-x world-y -1))
        origin (multiply-matrix-by-tuple (inverse (:transform camera)) (point 0 0 0))
        direction (normalize-vektor (subtract-tuples pixel origin))]
    (ray origin direction)))
(defn render
  ([camera world] (render camera world nil))
  ([camera world report-progress]
   (let [width (:hsize camera)
         height (:vsize camera)
         percentage (let [divider (/ (* width height) 100.0)] (fn [index] (/ (inc index) divider)))
         coords (for [x (range width) y (range height)] [x y])
         shape-to-parent (shape-to-parent (:objects world))
         color-at (fn [[x y]] [[x y] (color-at world (ray-for-pixel camera x y) shape-to-parent)])
         write-pixel (fn [canvas [index [x y] color]]
                       (do (if report-progress (report-progress (percentage index)))
                           (write-pixel canvas x y color)))]
     (reduce write-pixel (canvas width height) (->> coords (pmap color-at) (map-indexed (fn [index item] (concat [index] item))))))))
