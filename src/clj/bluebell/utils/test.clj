(ns bluebell.utils.test)

(defn near [a b tol]
  (< (Math/abs (double (- a b))) tol))
