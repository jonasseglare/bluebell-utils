(ns bluebell.fibers.ops-test
  (:refer-clojure :exclude [+ - / * inc dec])
  (:require [bluebell.fibers.ops :refer :all :as ops]
            [bluebell.fibers.types :as types]
            [bluebell.fibers.core :as core]
            [clojure.test :refer :all]
            [clojure.spec :as spec]
            [bluebell.utils.core :as utils]))

(deftest add-test
  (is (+ (types/primitive :double 3)
         (types/primitive :long 9)))
  (is (= (core/make-node (+ (types/primitive :double 3)) ['kattskit])
         '(clojure.core/unchecked-add kattskit)))
  (is (= (core/make-node (+ (types/primitive :double 3)
                            (types/primitive :int 9)) ['a 'b])
         '(clojure.core/unchecked-add a b))))

(deftest add-test-eval
  (is (= 7 (utils/macro-eval 
            (core/make-code (+ (types/primitive :double 3)
                               (types/primitive :double 4))))))
  (is (= 12 (core/expand
             (* (types/primitive :double 3)
                (types/primitive :double 4))))))
