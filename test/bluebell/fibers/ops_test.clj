(ns bluebell.fibers.ops-test
  (:refer-clojure :exclude [+ - / * inc dec])
  (:require [bluebell.fibers.ops :refer :all :as ops]
            [bluebell.fibers.types :as types]
            [bluebell.fibers.core :as core]
            [clojure.test :refer :all]
            [bluebell.utils.access :as access]
            [clojure.spec.alpha :as spec]
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

(deftest nested-add
  (is (= 7 (utils/macro-eval
            (core/make-code (+ (+ (types/input 3)
                                  (types/input 2))
                               (types/input 2)))))))

(defn static-if [cond true-branch false-branch]
  (core/node
   (access/build core/-datatype :unspecified
                 core/-args [cond true-branch false-branch])))

(comment
  (core/make-map {}
                 (static-if (types/input 1)
                            (types/input 2)
                            (types/input 3)))

  (core/make-code
   (static-if (types/input 1)
              (types/input 2)
              (types/input 3)))

  )

