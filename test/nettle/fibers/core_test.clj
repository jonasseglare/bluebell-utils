(ns nettle.fibers.core-test
  (:require [clojure.test :refer :all]
            [nettle.utils.access :as access]
            [nettle.fibers.core :refer :all :as egraph]
            [clojure.spec :as spec]
            [nettle.utils.defmultiple :refer [defmultiple-extra]]))

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
  (primitive-expr :mock-double x))

(defn make-dnum-op [nodetype]
  (fn [& args]
    (access/build -datatype :mock-double
                  -simple? false
                  -args args
                  -nodetype nodetype)))

(def test-add (make-dnum-op :test-add))
(def test-mul (make-dnum-op :test-mul))
(def test-div (make-dnum-op :test-div))
(def test-sqrt (make-dnum-op :test-sqrt))

(spec/def ::double-vector (spec/coll-of (node-of-type? :mock-double)))

(deftest is-double-vector-test
  (is (spec/valid? ::double-vector [(dnum 9) (dnum 3)])))

(defn normalize [v]
  (assert (spec/valid? ::double-vector v))
  (let [sum-squares (reduce test-add (map (fn [x] (test-mul x x)) v))
        norm (test-sqrt sum-squares)]
    (mapv (fn [x] (test-div x norm)) v)))

(def normalized (normalize [(dnum 3) (dnum 4)]))

(deftest normalization-test
  (is (every? node? normalized)))

(deftest expr-test
  (is (node? (dnum 9)))
  (is (not (node? 9)))
  (is (node? (test-add (dnum 9) (dnum 3)))))

(defn key-node-pair? [[k v]]
  (or (and (key? k) (node? v))
      (and (node? k) (key? v))))

(deftest expression-map-test
  (is (map? (add-simple-subexpr {} :mjao (dnum 9))))
  (let [expanded (add-subexpr {} :kattskit (test-add (dnum 3) (dnum 4)))
        argsyms (-> expanded :kattskit :args)]
    (assert (is (map? expanded)))
    (assert (every? key-node-pair? expanded))
    (assert (every? symbol? argsyms)))

  (is (= (make-map {} [9 3 4])
         [{} [9 3 4]]))
  
  (let [[small-map e] (make-map {} [(dnum 3) :a])]
    (is (map? small-map))
    (is (every? key-node-pair? small-map))
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
                          
(defn diamond [x]
  (assert (node? x))
  (test-add x x))

(deftest diamond-test 
  (let [mv (make-map {} (diamond (dnum 3)))
        [m v] mv
        m2 m] ;(inc-ref-recursive m v)]
    (let [values (set (map #(-> % second :refcount) m2))]
      (is (contains? values 2))
      (is (contains? values 1))
      (is (contains? values nil)))))

(defmultiple-extra make-node
  (:test-add [x args] `(+ ~@args))
  (:test-sub [x args] `(- ~@args))
  (:test-div [x args] `(/ ~@args))
  (:test-mul [x args] `(* ~@args))
  (:test-sqrt [x args] `(Math/sqrt ~@args)))


(deftest make-node-test
  (make-node (dnum 3) [])
  (is (= (make-node (test-add (dnum 3) (dnum 4)) [3 4 5])
         '(clojure.core/+ 3 4 5))))

(deftest binding-test
  (is (not (bind? (dnum 3))))
  (is (not (bind? (dnum '(+ 4 5))))))

(deftest make-code-test
  (is (= [0.6 0.8]
         (eval (make-code normalized)))))
