(ns nettle.fibers.ops-test
  (:refer-clojure :exclude [+ - / *])
  (:require [nettle.fibers.ops :refer :all]
            [nettle.fibers.types :as types]
            [nettle.fibers.core :as core]
            [clojure.test :refer :all]
            [nettle.utils.core :as utils]))

(deftest add-test
  (is (+ (types/primitive :double 3)
         (types/primitive :long 9)))
  (is (= (core/make-node (+ (types/primitive :double 3)) ['kattskit])
         '(clojure.core/unchecked-add kattskit)))
  (is (= (core/make-node (+ (types/primitive :double 3)
                            (types/primitive :int 9)) ['a 'b])
         '(clojure.core/unchecked-add a (clojure.core/unchecked-double b)))))
