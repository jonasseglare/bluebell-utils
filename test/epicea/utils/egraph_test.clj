(ns epicea.utils.egraph-test
  (:require [clojure.test :refer :all]
            [epicea.utils.access :as access]
            [epicea.utils.egraph :refer :all :as egraph]
            [clojure.spec :as spec]))

(deftest egraph-test-node?
  (is (node? {:unique-tag ::egraph/node :kattskit 9}))
  (is (node? empty-node))
  (is (not (node? {:unique-tag 9})))
  (is (node? (primitive-expr :double 9)))
  (let [a (primitive-expr :double 9)
        b (primitive-expr :double '(+ 3 4))]
    (is (access/get a -simple?))
    (is (not (access/get b -simple?)))))

(defn dnum [x]
  (primitive-expr :double x))

(defn make-dnum-op [maker]
  (fn [& args]
    (access/build -type :double
                  -simple? false
                  -make maker
                  -args args)))

(def test-add (make-dnum-op (fn [args] `(+ ~@args))))
(def test-mul (make-dnum-op (fn [args] `(* ~@args))))
(def test-div (make-dnum-op (fn [args] `(/ ~@args))))
(def test-sqrt (make-dnum-op (fn [args] `(sqrt ~@args))))

(spec/def ::double-vector (spec/coll-of (node-of-type? :double)))

(deftest is-double-vector-test
  (is (spec/valid? ::double-vector [(dnum 9) (dnum 3)])))

(defn normalize [v]
  (assert (spec/valid? ::double-vector v))
  (let [sum-squares (reduce test-add (map (fn [x] (test-mul x x)) v))
        norm (test-sqrt sum-squares)]
    (map (fn [x] (test-div x norm)) v)))

(deftest normalization-test
  (is (every? node? (normalize [(dnum 3) (dnum 4)]))))

(deftest expr-test
  (is (node? (dnum 9)))
  (is (not (node? 9)))
  (is (node? (test-add (dnum 9) (dnum 3)))))

(deftest expression-map-test
  (is (map? (add-simple-subexpr {} :mjao (dnum 9))))
  (let [expanded (add-subexpr {} :kattskit (test-add (dnum 3) (dnum 4)))
        argsyms (-> expanded :kattskit :args)]
    (assert (is (map? expanded)))
    (assert (every? (fn [[k v]]
                      (and (key? k)
                           (node? v))) expanded))
    (assert (every? symbol? argsyms)))

  (is (= (make-map {} [9 3 4])
         [{} [9 3 4]]))
  
  (let [[small-map e] (make-map {} [(dnum 3) :a])]
    (is (map? small-map))
    (is (every? (fn [[k v]]
                  (and (key? k)
                       (node? v))) small-map))
    (is (= 2 (count e)))
    (is (key? (first e)))
    (is (= :a (second e)))
    (is (seq? (second (make-map {} (list (dnum 3) :a)))))
    (is (symbol? (-> (make-map {} {:a (dnum 3) :b 9})
                     second
                     :a)))))

(deftest refcount-test
  (let [d-with-ref (access/set (dnum 3) -refcount 9)]
    (is (not (access/has? (:a (reset-refcount {:a d-with-ref}))
                          -refcount)))))
                          

