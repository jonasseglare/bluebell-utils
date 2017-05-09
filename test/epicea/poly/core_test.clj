(ns epicea.poly.core-test
  (:require [epicea.poly.core :refer :all :as poly]
            [clojure.test :refer :all]
            [epicea.utils.access :as access]
            [epicea.utils.optional :as optional]
            [clojure.spec :as spec]  :reload-all))



(def args ['a [:pred number?] 'b 'c '& 'd])
(def args2 ['a [:pred number?] 'b])

(def parsed (spec/conform ::poly/arglist args))
(def parsed2 (spec/conform ::poly/arglist args2))

(deftest regroup-test
  (is (= [0 1 2 [3 4]] (regroup-args parsed (range 5))))
  (is (= [0 1 2 []] (regroup-args parsed (range 3))))
  (is (nil? (regroup-args parsed (range 2))))
  (is (= [0 1] (regroup-args parsed2 (range 2))))
  (is (nil? (regroup-args parsed2 (range 1))))
  (is (nil? (regroup-args parsed2 (range 3)))))

(deftest get-expr-bindings-test
  (is (= ['a 'b 'c] (get-main-expr-bindings parsed)))
  (is (= ['a 'b 'c 'd] (get-arglist-bindings parsed)))
  (is (= ['a 'b] (get-arglist-bindings parsed2))))

(deftest test-get-expr
  (is (= ['a] (get-expr-bindings 
               (spec/conform 
                ::poly/expr 
                [:get number? 'a])))))

(def katt (access/map-accessor :katt))

(def test-expr (spec/conform ::poly/expr [:access katt 'a]))
(def test-expr2 (spec/conform ::poly/expr [:get inc 'a 'b]))
(def test-expr3 (spec/conform ::poly/expr [:get inc 'a 'b 'c]))
(def test-expr4 (spec/conform ::poly/expr [:get inc 'a 'b [:get inc 'c]]))
(def test-expr5 (spec/conform ::poly/expr [:access katt 'a]))
(def test-expr6 (spec/conform ::poly/expr [[:pred number?] 'a]))

(def test-expr7 (spec/conform ::poly/expr [[:get :mjao [:pred number?] 'a] 
                                           [:access katt [:get :skit 'b]]]))

(deftest get-exprs-test
  (is (vector? (get-exprs test-expr7)))
  (is (= [[:binding 'a]] (get-exprs test-expr5))))


(deftest test-access 
  (is (= [3] (eval-optional test-expr {:katt 3})))
  (is (= [121] (eval-optional test-expr2 120)))
  (is (= [10 10 10] (eval-expr-bindings [] test-expr3 9)))
  (is (= [10 10 11] (eval-expr-bindings [] test-expr4 9)))
  (is (= [119] (eval-expr-bindings [] test-expr5 {:katt 119})))
  (is (= [9] (eval-expr-bindings [] test-expr6 9)))
  (is (nil? (eval-expr-bindings [] test-expr6 :a)))
  (is (= ['a] (get-expr-bindings test-expr6)))
  (is (= ['a] (get-expr-bindings test-expr5)))
  (is (= ['a 'b 'c] (get-expr-bindings test-expr3)))
  (is (= ['a 'b] (get-expr-bindings test-expr7)))
  (is (= [9 3] (eval-expr-bindings 
                [] test-expr7 
                {:mjao 9 :katt {:skit 3}})))
  (is (nil? (eval-expr-bindings [] test-expr7 {:mjao 9})))
  (is (= [[:binding 'a] [:binding 'b]]
         (access/getx expr-x (spec/conform ::poly/expr ['a 'b]))))
  (is (nil? (eval-expr-bindings [] test-expr7 {:mjao :a :katt {:skit 3}}))))

(def expr8 (spec/conform ::poly/expr [[:pred 'number?] 'a]))

(deftest compile-test
  (is (= [[:group [[:predicate {:prefix :pred, 
                                :fn number?}]
                   [:binding 'a]]]]
         (compile-exprs [expr8]))))

(def expr10 (spec/conform ::poly/arglist ['a [[:pred number?] 'b]]))

(defn vector-of-numbers? [x]
  (and (vector? x)
       (every? number? x)))

(def expr11 (spec/conform ::poly/arglist ['a [[:pred number?] 'b] 
                                          '& [[:pred vector-of-numbers?] 'c]]))

(deftest compile-arglist
  (is (= (compile-arglist-exprs expr11)
         {:main [[:binding 'a] 
                 [:group [[:predicate {:prefix :pred, :fn number?}] 
                          [:binding 'b]]]], 
          :rest {:and '&, 
                 :args [:group [[:predicate 
                                 {:prefix :pred, :fn vector-of-numbers?}] 
                                [:binding 'c]]]}}))
  (is (= (compile-arglist-exprs expr10)
         {:main [[:binding 'a] 
                 [:group [[:predicate {:prefix :pred, :fn number?}] 
                          [:binding 'b]]]]})))
                                


(def f10 (compile-arg-parser (compile-arglist-exprs expr10)))

(deftest compile-args-test
  (is (= [3 4] (f10 [3 4])))
  (is (nil? (f10 [3 :a])))
  (is (nil? (f10 [3])))
  (is (nil? (f10 [3 4 5]))))

(def expr11c (compile-arglist-exprs expr11))
(def f11 (compile-arg-parser expr11c))

(deftest compile-args-test2
  (is (= [3 4 [5]] (f11 [3 4 5])))
  (is (= [3 4 [5 6]] (f11 [3 4 5 6])))
  (is (nil? (f11 [3 4 :a]))))

(def fn10 (compile-body-fun expr10 ['(+ a b)]))

(deftest body-fn-test
  (is (= (optional/optional 17) 
         (fn10 [9 8])))
  (is (= (optional/optional)
         (fn10 [9 :a]))))

(deftest defpoly-parse
  (is (not= ::spec/invalid (spec/conform ::poly/defpoly '(kattskit))))
  (is (not= ::spec/invalid (spec/conform 
                            ::poly/defpoly 
                            '(kattskit :default :kattskit))))
  (is (not (spec/valid? ::poly/defpoly 
                        '(kattskit :kattskit))))
  (is (spec/valid? ::poly/method
                   '([a b] inc)))
  (is (not (spec/valid? ::poly/method
                        '(a inc))))
  (is (spec/valid? ::poly/method
                   '([a b] (+ a b) (* a b))))
  (is (spec/valid? ::poly/method
                   '([a b])))
  (is (spec/valid? ::poly/defpoly
                   '(kattskit :default mjao
                      ([a b] (+ a b))
                      ([a] (* a a)))))
  (is (spec/valid? ::poly/defpoly
                   '(kattskit :default mjao
                      ([[[:pred number?] a] b] (+ a b))
                      ([a] (* a a))))))
(defpoly my-add
  ([[[:pred keyword?] a]] [:keyword a])
  ([[[:pred number?] aa]
    [[:pred number?] bb]]
   (+ aa bb))
  ([aa bb] (str aa " + " bb)))

(defpoly-extra my-add
  ([a b c] (+ a b c)))

(deftest my-add-test
  (is (= [:keyword :a] (my-add :a)))
  (is (= 12 (my-add 9 3)))
  (is (= "a + b" (my-add "a" "b")))
  (is (= 12 (my-add 3 4 5))))
