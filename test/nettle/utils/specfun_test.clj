(ns nettle.utils.specfun-test
  (:require [nettle.utils.specfun :refer :all :as specfun]
            [clojure.test :refer :all]
            [clojure.spec :as spec]))

(deftest various-spec
  (is (spec/valid? ::specfun/def '(my-spec name (+ 1 2) (+ 4 5))))
  (is (spec/valid? ::specfun/defs '((a avar (+ 3 4) (+ 5 6))
                                    (b bvar (+ 3 4) (+ 5 6))))))
