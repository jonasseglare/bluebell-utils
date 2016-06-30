(ns epicea.utils)

(defn map-with-keys? [x ks]
  (if (map? x)
    (every? (fn [v] (contains? ks v)) (keys x))))
