(ns bluebell.bubble)

(defn bubble [x]
  [::bubble x])

(defn bubble? [x]
  (and (vector? x)
       (= ::bubble (first x))))

(defn break [x]
  (assert (bubble? x))
  (second x))

(defn protect-fn [f]
  (fn [& args]
    (or (first (filter bubble? args))
        (apply f args))))

