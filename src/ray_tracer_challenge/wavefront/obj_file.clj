(ns ray-tracer-challenge.wavefront.obj-file
  (:require [clojure.string :as str]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.triangles :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn get-vertex [parsed-file index] (get (:vertices parsed-file) (dec index)))
(defn get-group [parsed-file name] (get-in parsed-file [:groups name]))
(defn parse-obj-file [file]
  (letfn [(parseDoubles [args] (map #(Double/parseDouble %) args))
          (parseInts [args] (map #(Integer/parseInt %) args))
          (process-vertex [result args] (update result :vertices conj (apply point (parseDoubles args))))
          (process-face [result args]
            (let [[p1 & ps] (map #(get-vertex result %) (parseInts args))
                  group-path (if-let [group-name (:last-group-name result)] [:groups group-name] [:default-group])]
              (reduce (fn [result [p2 p3]] (update-in result group-path (fnil conj []) {:p1 p1 :p2 p2 :p3 p3}))
                      result (partition 2 1 ps))))
          (process-group [result args] (assoc result :last-group-name (first args)))
          (ignore-line [result line] (update result :ignored-lines conj line))
          (process-line [result line]
            (let [[command & args] (str/split line #"\s+")]
              (condp = command
                "v" (process-vertex result args)
                "f" (process-face result args)
                "g" (process-group result args)
                (ignore-line result line))))]
    (reduce process-line
            {:vertices      []
             :default-group []
             :groups        {}
             :ignored-lines []}
            (filter (comp not str/blank?) (str/split-lines file)))))
(defn obj-to-group [parsed-file]
  (group :children (->> (concat [(:default-group parsed-file)] (vals (:groups parsed-file)))
                        (mapv (fn [obj-group] (mapv #(triangle (:p1 %) (:p2 %) (:p3 %)) obj-group)))
                        (mapv #(group :children %)))))
