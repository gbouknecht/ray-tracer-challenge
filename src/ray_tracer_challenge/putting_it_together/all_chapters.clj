(ns ray-tracer-challenge.putting-it-together.all-chapters
  (:require [clojure.java.io :refer [file]]))

(defn- filter-on-chapter [get-name coll] (filter #(re-find #"chapter[-_]\d\d[-_]" (get-name %)) coll))

(defn -main []
  (doseq [file (->> (file-seq (file "."))
                    (filter-on-chapter #(.getName %)))]
    (load-file (.getPath file)))
  (doseq [chapter-ns (->> (all-ns)
                          (filter-on-chapter (comp name ns-name))
                          (sort-by (comp name ns-name)))]
    (println (str "Running " chapter-ns))
    (apply (ns-resolve chapter-ns (symbol "-main")) nil))
  (println "Finished"))
