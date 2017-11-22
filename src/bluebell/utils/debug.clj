(ns bluebell.utils.debug
  (:require [clojure.pprint]))

(defn dout-sub [label x]
  `(let [x# ~x]
     (println (str "--> " ~label " ="))
     (clojure.pprint/pprint x#)
     x#))

(defmacro dout 
  ([label x]
   (dout-sub (str label)  x))
  ([x] (dout-sub (str x) x)))

(defmacro douts [& args]
  `(do ~@(map (fn [arg] `(dout ~arg)) args)))

(defn disp-fn-io
  ([f] (disp-fn-io f "Unknown function"))
  ([f label]
   (fn [& args]
     (let [output (apply f args)]
       (println "----Function " label)
       (println "Inputs")
       (doseq [x args]
         (println "Arg->")
         (clojure.pprint/pprint x))
       (println "  ---> Output: ")
       (clojure.pprint/pprint output)
       output))))

(defmacro with-pprint-code [& args]
  `(binding [clojure.pprint/*print-pretty* clojure.pprint/code-dispatch]
    ~@args))

(defn pprint-code [& args]
  (with-pprint-code
    (apply clojure.pprint/pprint args)))
