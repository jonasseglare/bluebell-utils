(ns epicea.poly.core-test
  (:require [epicea.poly.core :refer :all :as poly]
            [clojure.test :refer :all]
            [epicea.utils.access :as access]
            [clojure.spec :as spec]  :reload-all))

(declpoly kattskit (fn [& _] :no-impl))

(deftest decl-def-poly
  (is (= :no-impl (kattskit 3))))


(def args ['a [:pred number?] 'b 'c '& 'd])
(def args2 ['a [:pred number?] 'b])

(def parsed (spec/conform ::poly/arglist args))
(def parsed2 (spec/conform ::poly/arglist args2))

(deftest get-expr-bindings-test
  (is (= ['a 'b 'c] (get-exprs-bindings (:main parsed))))
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
  (is (vector? (get-exprs test-expr7))))

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



  

