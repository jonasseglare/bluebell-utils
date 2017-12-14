(ns bluebell.utils.debug
  (:require [clojure.pprint :as pp]))

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

(defn cmp-ab [x]
  (assoc x :eq (= (:a x) (:b x))))

(declare explain-diff)

(defn explain-diff-maps [a b]
  (str "\nBoth are maps"
       (let [ks (set (keys a))]
         (if (= ks (set (keys b)))
           (str "\n with the same set of keys"
                (let [f (first
                         (filter (complement :eq)
                                 (map (fn [k]
                                        (cmp-ab {:key k
                                                 :a (get a k) 
                                                 :b (get b k)}))
                                      ks)))]
                  (str "\nBut different values at "
                       (with-out-str
                         (clojure.pprint/pprint (:key f)))
                       "Compare them there:"
                       (explain-diff (:a f) (:b f)))))
           (str "\n with different set of keys"
                "\n  a: " (with-out-str (pp/pprint (sort (keys a))))
                "\n  b: " (with-out-str (pp/pprint (sort (keys b)))))))))


(defn explain-diff-sequentials [a b]
  (str "\nBoth are sequentials"
       (if (= (count a) (count b))
         (str "\nOf the same length"
              (let [f (first (filter (complement :eq)
                                     (map (fn [i a b]
                                            (cmp-ab
                                             {:index i
                                              :a a
                                              :b b}))
                                          (range (count a))
                                          a b)))]
                (str "\nBut their elements at " (:index f) " differ:"
                     (explain-diff (:a f) (:b f)))))
         (str "\nOf different lengths: " (count a) " and " (count b)))))

(defn explain-diff-sets [a b]
  (let [amb (clojure.set/difference a b)
        bma (clojure.set/difference b a)]
    (str "\nBoth are sets but"
         (if (empty? amb) "" (str  "\nOnly a contains " (with-out-str (pp/pprint amb))))
         (if (empty? bma) "" (str  "\nOnly b contains " (with-out-str (pp/pprint bma)))))))

(defn limit-string [n s]
  (if (<= (count s) n) s (str (subs s 0 n) "...")))

(def max-diff-len 30)

(defn explain-diff [a b]
  (if
    (= a b) "Identitical"
    (str "\nThey are different"
         (cond
           (and (map? a)
                (map? b)) (explain-diff-maps a b)
           (and (sequential? a)
                (sequential? b)) (explain-diff-sequentials a b)
           (and (set? a) (set? b)) (explain-diff-sets a b)
           :default (str "\nThey have different type"
                         "\na = \n" (limit-string max-diff-len (with-out-str (pp/pprint a)))
                         "\nb = \n" (limit-string max-diff-len (with-out-str (pp/pprint b))))))))
