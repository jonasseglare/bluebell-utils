(ns epicea.poly.core-test
  (:require [epicea.poly.core :refer :all]
            [clojure.test :refer :all]))

(declpoly kattskit (fn [& _] :no-impl))

(deftest decl-def-poly
  (is (= :no-impl (kattskit 3))))
  
