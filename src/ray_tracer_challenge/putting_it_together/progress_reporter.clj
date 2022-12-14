(ns ray-tracer-challenge.putting-it-together.progress-reporter)

(defn make-report-progress-fn []
  (let [start-time (System/currentTimeMillis)]
    (fn [percentage]
      (let [percentage (int percentage)
            elapsed-time (- (System/currentTimeMillis) start-time)]
        (do (printf "\rprogress: %3d%%, elapsed time: %02d:%02d:%02d"
                    percentage
                    (int (/ elapsed-time 1000 60 60))
                    (rem (int (/ elapsed-time 1000 60)) 60)
                    (rem (int (/ elapsed-time 1000)) 60))
            (flush))
        (if (= percentage 100) (println))))))
