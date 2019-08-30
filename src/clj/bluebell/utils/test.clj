(ns bluebell.utils.test)

(defn near [a b tol]
  (< (Math/abs (- a b)) tol))
