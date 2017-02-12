(ns epicea.utils.debug)

(defn dout-sub [label x]
  `(let [x# ~x]
     (println (str "\n<<<<<<<<<<<<<<<<<(" ~label ")---------"))
     (println ~(str x) "=\n" x#)
     (println ">>>>>>>>>>>>>>>>\n")
     x#))

(defmacro dout 
  ([label x]
   (dout-sub (str label) x))
  ([x] (dout-sub (str x) x)))
