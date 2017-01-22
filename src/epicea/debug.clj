(ns epicea.debug)

(defmacro dout [x]
  `(let [x# ~x]
     (println ~(str x) "=" x#)
     x#))
