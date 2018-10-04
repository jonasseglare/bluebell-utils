(ns bluebell.utils.wip.timelog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn current-time-seconds []
  (* 0.001 (System/currentTimeMillis)))

(defn timelog
  ([label start-time-seconds]
   {:pre [(number? start-time-seconds)]}
   [[label start-time-seconds]])
  ([start-time-seconds]   
   (timelog "Start" start-time-seconds))
  ([] (timelog (current-time-seconds))))

(defn log
  ([dst data time]
   {:pre [(vector? dst)
         (number? time)]}
   (conj dst [data time]))
  ([dst data]
   {:pre [(vector? dst)]}
   (log dst data (current-time-seconds))))

(defn offset-normalize [data]
  {:pre [(vector? data)]}
  (let [[_ offset] (first data)]
    (mapv (fn [[label t]]
            [label (- t offset)])
          data)))

(defn total-time [src]
  (- (-> src last second)
     (-> src first second)))

(defn disp [src]
  {:pre [(vector? src)]}
  (doseq [[label t] (offset-normalize src)]
    (println (format "%s: %.3g" label t))))
