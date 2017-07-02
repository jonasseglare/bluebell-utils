(ns bluebell.utils.failure)

(defn failure [x]
  [::failure x])

(defn failure? [x]
  (and (vector? x)
       (= ::failure (first x))))

(defn failure-value [x]
  (assert (failure? x))
  (second x))

(defn wrapf [f]
  (fn [& args]
    (or (first (filter failure? args))
        (apply f args))))
