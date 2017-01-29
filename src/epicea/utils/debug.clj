(ns epicea.utils.debug)

(defmacro dout [x]
  `(let [x# ~x]
     (println ~(str x) "=" x#)
     x#))
