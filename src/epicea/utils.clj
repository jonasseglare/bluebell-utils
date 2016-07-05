(ns epicea.utils)

(defn map-with-keys? [x ks]
  (if (map? x)
    (every? (fn [v] (contains? ks v)) (keys x))))

(defn compute-matrix-index [sizes indices]
  (assert (= (count sizes)
             (count indices)))
  (loop [S (vec sizes)
         I (vec indices)
         i 0]
    (if (empty? I)
      i
      (recur (pop S) (pop I) (+ (* i (last S)) (last I))))))
      
;; Macro to define an initial value and the keys
(defmacro def-map [map-name empty-map]
  `(do
     (def ~(symbol (str "empty-" map-name)) ~empty-map)
     (defn ~(symbol (str map-name "?")) [x#]
       (map-with-keys? x# ~(set (keys empty-map))))))
       
(defn provide-argument [fun index value]
  (fn [& args0]
    (let [args (vec args0)
          args2 (concat (subvec args 0 index) [value] (subvec args index))]
      (apply fun args2))))
