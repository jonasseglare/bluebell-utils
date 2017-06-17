(ns bluebell.utils.optional)

(defn optional 
  ([] nil)
  ([x] [x]))

(defn optional? [x]
  (or (nil? x)
      (and (vector? x) (= 1 (count x)))))
