(ns ray-tracer-challenge.test.test-utils
  (:require [clojure.test :refer :all]
            [ray-tracer-challenge.logic.common :refer :all]
            [ray-tracer-challenge.logic.intersections :refer :all]
            [ray-tracer-challenge.logic.matrices :refer :all]))

(declare roughly not-roughly not-thrown?)

(defn diff-smaller-than-epsilon? [x y]
  (cond (and (matrix? x) (matrix? y)) (diff-smaller-than-epsilon? (:values x) (:values y))
        (and (vector? x) (vector? y)) (and (= (count x) (count y)) (every? #(apply diff-smaller-than-epsilon? %) (map vector x y)))
        (and (number? x) (number? y)) (< (abs (- x y)) epsilon)
        (and (intersection? x) (intersection? y)) (and (diff-smaller-than-epsilon? (:t x) (:t y)) (= (:object x) (:object y)))
        :else (throw (IllegalArgumentException. (format "Arguments not supported: %s (%s), %s (%s)" x (type x) y (type y))))))

(defn format-value [value] (if (matrix? value) (str-matrix value) (str value)))

(defmethod assert-expr 'roughly [msg form]
  `(let [x# ~(nth form 1)
         y# ~(nth form 2)
         result# (diff-smaller-than-epsilon? x# y#)]
     (do-report
       {:type     (if result# :pass :fail)
        :message  ~msg
        :expected (format-value x#)
        :actual   (format-value y#)})
     result#))

(defmethod assert-expr 'not-roughly [msg form]
  `(let [x# ~(nth form 1)
         y# ~(nth form 2)
         result# (not (diff-smaller-than-epsilon? x# y#))]
     (do-report
       {:type     (if result# :pass :fail)
        :message  ~msg
        :expected (str "not " (format-value x#))
        :actual   (format-value y#)})
     result#))

(defmethod assert-expr 'not-thrown? [msg form]
  (let [klass (second form)
        body (nthnext form 2)]
    `(try ~@body
          (do-report {:type     :pass, :message ~msg,
                      :expected (str "not " '~form), :actual nil})
          (catch ~klass e#
            (do-report {:type     :fail, :message ~msg,
                        :expected (str "not " '~form), :actual e#})
            e#))))