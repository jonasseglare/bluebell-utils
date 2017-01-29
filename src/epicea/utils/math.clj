(ns epicea.utils.math)

(defn sqr [x] (* x x))

(defn extend-bounds [a b]
  (cond
    (= [] a) b
    (= [] b) a
    :default 
    (let [[l0 u0] a
          [l1 u1] b]
      [(Math/min l0 l1)
       (Math/max u0 u1)])))
  
(defn clamp [bounds x]
  (if (= [] bounds) x
      (let [[a b] bounds]
        (Math/min b (Math/max a x)))))
