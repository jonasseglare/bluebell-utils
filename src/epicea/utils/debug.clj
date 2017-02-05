(ns epicea.utils.debug)

(defn dout-sub [label x]
  `(let [x# ~x]
     (println "DOUT (" label ")---------")
     (println ~(str x) "=" x#)
     x#))

(defmacro dout 
  ([label x]
   (dout-sub label x))
  ([x] (dout-sub "-x" x)))
