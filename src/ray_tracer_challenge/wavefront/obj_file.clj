(ns ray-tracer-challenge.wavefront.obj-file
  (:require [clojure.string :as str]
            [ray-tracer-challenge.logic.shapes :refer :all]
            [ray-tracer-challenge.logic.triangles :refer :all]
            [ray-tracer-challenge.logic.tuples :refer :all]))

(defn get-vertex [parsed-file index] (get (:vertices parsed-file) (dec index)))
(defn get-vertex-normal [parsed-file index] (get (:vertex-normals parsed-file) (dec index)))
(defn get-group [parsed-file name] (get-in parsed-file [:groups name]))
(defn parse-obj-file [file]
  (letfn [(parse-doubles [args] (map #(Double/parseDouble %) args))
          (parse-ints [args] (map #(if (not (str/blank? %)) (Integer/parseInt %)) args))
          (parse-vertex-triplets [args] (map #(parse-ints (str/split % #"/")) args))
          (process-vertex [result args] (update result :vertices conj (apply point (parse-doubles args))))
          (process-normal [result args] (update result :vertex-normals conj (apply vektor (parse-doubles args))))
          (process-face [result args]
            (let [[v1 & vs] (->> args
                                 (parse-vertex-triplets)
                                 (map (fn [[v-index _ vn-index]] {:p (get-vertex result v-index)
                                                                  :n (if vn-index (get-vertex-normal result vn-index))})))
                  group-path (if-let [group-name (:last-group-name result)] [:groups group-name] [:default-group])]
              (reduce (fn [result [v2 v3]] (update-in result group-path (fnil conj []) {:p1 (:p v1) :p2 (:p v2) :p3 (:p v3)
                                                                                        :n1 (:n v1) :n2 (:n v2) :n3 (:n v3)}))
                      result (partition 2 1 vs))))
          (process-group [result args] (assoc result :last-group-name (first args)))
          (ignore-line [result line] (update result :ignored-lines conj line))
          (process-line [result line]
            (let [[command & args] (str/split line #"\s+")]
              (condp = command
                "v" (process-vertex result args)
                "vn" (process-normal result args)
                "f" (process-face result args)
                "g" (process-group result args)
                (ignore-line result line))))]
    (reduce process-line
            {:vertices       []
             :vertex-normals []
             :default-group  []
             :groups         {}
             :ignored-lines  []}
            (filter (comp not str/blank?) (str/split-lines file)))))
(defn obj-to-group [parsed-file & {:keys [transform]}]
  (letfn [(make-triangle [t]
            (let [[p1 p2 p3] ((juxt :p1 :p2 :p3) t)
                  [n1 n2 n3] ((juxt :n1 :n2 :n3) t)]
              (if (and n1 n2 n3)
                (smooth-triangle p1 p2 p3 n1 n2 n3)
                (triangle p1 p2 p3))))]
    (group :transform transform
           :children (->> (concat [(:default-group parsed-file)] (vals (:groups parsed-file)))
                          (mapv (fn [obj-group] (mapv make-triangle obj-group)))
                          (mapv #(group :children %))))))
