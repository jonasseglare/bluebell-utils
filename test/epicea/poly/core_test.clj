(ns epicea.poly.core-test
  (:require [epicea.poly.core :refer :all :as poly]
            [clojure.test :refer :all]
            [clojure.spec :as spec]  :reload-all))

(declpoly kattskit (fn [& _] :no-impl))

(deftest decl-def-poly
  (is (= :no-impl (kattskit 3))))


(def args ['a [:pred number?] 'b 'c '& 'd])

(def parsed (spec/conform ::poly/arglist args))
(deftest get-expr-bindings-test
  (is (= ['a 'b 'c] (get-exprs-bindings (:main parsed))))
  (is (= ['a 'b 'c 'd] (get-arglist-bindings parsed))))
