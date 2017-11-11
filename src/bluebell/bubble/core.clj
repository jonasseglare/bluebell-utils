(ns bluebell.bubble.core)

(defn bubble [x]
  [::bubble x])

(defn bubble? [x]
  (and (vector? x)
       (= ::bubble (first x))))

(defn break [x]
  (assert (bubble? x))
  (second x))

(defn protect-apply [f & args]
  (or (first (filter bubble? args))
      (apply f args)))

(defn protect-fn [f]
  (partial protect-apply f))

(defn to-nil-or-vec [x]
  (if (not (bubble? x))
    [x]))

(defmacro alts [& args]
  `(first (or ~@(map (fn [x] `(to-nil-or-vec ~x)) args))))

