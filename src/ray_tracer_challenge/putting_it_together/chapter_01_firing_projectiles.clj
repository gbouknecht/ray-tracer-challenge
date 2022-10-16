(ns ray-tracer-challenge.putting-it-together.chapter-01-firing-projectiles
  (:require [clojure.string :as str]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn tick [{gravity :gravity wind :wind} {position :position velocity :velocity}]
  (let [new-position (add-tuples position velocity)
        new-velocity (reduce add-tuples [velocity gravity wind])]
    {:position new-position :velocity new-velocity}))

(defn fire [environment projectile]
  (take-while (fn [[_ y _]] (pos? y)) (map :position (iterate #(tick environment %) projectile))))

(defn fire-and-print [{gravity :gravity wind :wind :as environment}
                      {position :position velocity :velocity :as projectile}]
  (letfn [(format-xyz [[x y z]] (format "(%5.2f, %5.2f, %5.2f)" (double x) (double y) (double z)))]
    (do
      (println (str/join (repeat 80 "=")))
      (println (format "       gravity: %s" (format-xyz gravity)))
      (println (format "          wind: %s" (format-xyz wind)))
      (println (format "start position: %s" (format-xyz position)))
      (println (format "start velocity: %s" (format-xyz velocity)))
      (println (str/join (repeat 80 "-")))
      (doseq [[index position] (map-indexed vector (fire environment projectile))]
        (println (format "%3d %s" (inc index) (format-xyz position)))))))

(defn -main []
  (fire-and-print {:gravity (vektor 0 -0.1 0) :wind (vektor -0.01 0 0)}
                  {:position (point 0 1 0) :velocity (normalize-vektor (vektor 1 1 0))})
  (fire-and-print {:gravity (vektor 0 -0.05 0) :wind (vektor -0.01 0 0)}
                  {:position (point 0 1 0) :velocity (normalize-vektor (vektor 1 1 0))}))
