(ns ray-tracer-challenge.putting-it-together.progress-reporter
  (:import (java.util.concurrent TimeUnit)))

(defn report-progress [percentage elapsed-time-in-ms estimated-time-left-in-ms]
  (let [hh-mm-ss #(if % [(.toHours TimeUnit/MILLISECONDS %)
                         (rem (.toMinutes TimeUnit/MILLISECONDS %) 60)
                         (rem (.toSeconds TimeUnit/MILLISECONDS %) 60)])]
    (print (apply format "\rprogress: %3d%%, elapsed time: %02d:%02d:%02d" (int percentage) (hh-mm-ss elapsed-time-in-ms)))
    (when-let [estimated-time-left-in-hh-mm-ss (hh-mm-ss estimated-time-left-in-ms)]
      (print (apply format ", estimated time left: %02d:%02d:%02d" estimated-time-left-in-hh-mm-ss)))
    (if (= (int percentage) 100) (println)))
  (flush))
