(ns bluebell.utils.debug
  (:require [clojure.pprint :as pp]
            [clojure.set]
            [clojure.spec.alpha :as spec]
            [clojure.string :as cljstr]))

(defn limit-string [n s]
  (if (<= (count s) n) s (str (subs s 0 n) "...")))

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

(defmacro dout-code [expr]
  `(let [x# ~expr]
     (println (str "The expr '" (limit-string 30 (quote ~expr))
                   "' is\n" (with-out-str (pprint-code x#))))
     x#))

(defn cmp-ab [x]
  (assoc x :eq (= (:a x) (:b x))))

(declare explain-diff)

(defn explain-diff-maps [a b]
  (str "\nBoth are maps"
       (let [aks (set (keys a))
             bks (set (keys b))]
         (if (= aks bks)
           (str "\n with the same set of keys"
                (let [f (first
                         (filter (complement :eq)
                                 (map (fn [k]
                                        (cmp-ab {:key k
                                                 :a (get a k) 
                                                 :b (get b k)}))
                                      aks)))]
                  (str "\nBut different values at "
                       (with-out-str
                         (clojure.pprint/pprint (:key f)))
                       "Compare them there:"
                       (explain-diff (:a f) (:b f)))))
           (str "\n with different set of keys:"
                (explain-diff aks bks))))))


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

(defn limited-pprint
  ([x] (limited-pprint x 300))
  ([x n]
   (println (limit-string
             n
             (with-out-str
               (pp/pprint x))))))

(def ^:dynamic dout-max-len 2000)

(defn dout-arg [arg]
  `(let [x# ~arg]
     (println ~(str "dout of '" arg "'"))
     (limited-pprint x# dout-max-len)
     (println "")
     x#))

(defmacro dout [& args]
  `(do
     ~@(map dout-arg args)))


(def todo-directives #{:break :done :sort-of-done :bug :ignore})
(def todo-directive? (partial contains? todo-directives))
(spec/def ::todo-directive todo-directive?)
(spec/def ::todo-directives (spec/* ::todo-directive))
(spec/def ::todo-message string?)
(spec/def ::todo-messages (spec/* ::todo-message))
(spec/def ::todo-syntax (spec/cat :directives ::todo-directives
                                  :messages ::todo-messages))

(defmacro TODO [& msg]
  (let [p (spec/conform ::todo-syntax msg)]
    (when (= ::spec/invalid p)
      (throw (ex-info "Bad TODO syntax"
                      {:explanation (spec/explain-str ::todo-syntax msg)})))

    (let [dirs (-> p :directives set)
          msgs (-> p :messages)
          marker (if (contains? dirs :bug)
                   "! ! ! !"
                   "-------") 
          prefix (str marker "> TODO: ")
          s (str prefix (cljstr/join (str "\n"
                                          (apply str (take (count prefix)
                                                           (repeat " "))))
                                     msgs))]
      (cond
        (or (contains? dirs :done)
            (contains? dirs :sort-of-done)
            (contains? dirs :ignore)) nil
        (contains? dirs :break) (throw (ex-info s {:message msgs}))
        :default (println s)))))

(defmacro pprint-macro [expr]
  `(-> (quote ~expr)
       macroexpand
       pprint-code))
