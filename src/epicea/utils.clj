(ns epicea.utils)

(defn map-with-keys? [x ks]
  (if (map? x)
    (every? (fn [v] (contains? ks v)) (keys x))))

(defn compute-matrix-index [sizes indices]
  (assert (count sizes)
          (count indices))
  (loop [S (vec sizes)
         I (vec indices)
         i 0]
    (if (empty? I)
      i
      (recur (pop S) (pop I) (+ (* i (last S)) (last I))))))
      
