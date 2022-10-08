(ns ray-tracer-challenge.test.test-utils
  (:require [clojure.test :refer :all]))

(declare roughly not-roughly)

(def epsilon 0.00001)

(defn diff-smaller-than-epsilon? [x y]
  (cond (and (vector? x) (vector? y)) (and (= (count x) (count y)) (every? #(apply diff-smaller-than-epsilon? %) (map vector x y)))
        :else (< (abs (- x y)) epsilon)))

(defmethod assert-expr 'roughly [msg form]
  `(let [x# ~(nth form 1)
         y# ~(nth form 2)
         result# (diff-smaller-than-epsilon? x# y#)]
     (do-report
       {:type (if result# :pass :fail)
        :message ~msg
        :expected (format "%s should be roughly %s (epsilon = %s)" x# y# epsilon)
        :actual result#})
     result#))

(defmethod assert-expr 'not-roughly [msg form]
  `(let [x# ~(nth form 1)
         y# ~(nth form 2)
         result# (not (diff-smaller-than-epsilon? x# y#))]
     (do-report
       {:type (if result# :pass :fail)
        :message ~msg
        :expected (format "%s should not even be roughly %s (epsilon = %s)" x# y# epsilon)
        :actual result#})
     result#))
