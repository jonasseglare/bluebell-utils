(ns nettle.utils.debug)

(defn dout-sub [label x]
  `(let [x# ~x]
     (println (str "--> " ~label " ="))
     (clojure.pprint/pprint x#)
     x#))

(defmacro dout 
  ([label x]
   (dout-sub (str label) x))
  ([x] (dout-sub (str x) x)))

(defmacro douts [& args]
  `(do ~@(map (fn [arg] `(dout ~arg)) args)))
