(ns bluebell.utils.specfun-test
  (:require [bluebell.utils.specfun :refer :all :as specfun]
            [clojure.test :refer :all]
            [clojure.spec :as spec]))

(deftest various-spec
  (is (spec/valid? ::specfun/def '(my-spec [name] (+ 1 2) (+ 4 5))))
  (is (spec/valid? ::specfun/defs '((a [avar] (+ 3 4) (+ 5 6))
                                    (b [bvar] (+ 3 4) (+ 5 6))))))

(spec/def ::numbers (spec/* number?))
(spec/def ::strings (spec/* string?))
(spec/def ::vectors (spec/* vector?))

(defspecfun add-any 
  (::numbers [x] (apply + x))
  (::strings [x] (apply str x))
  (::vectors [x] (reduce into [] x)))

(deftest add-any-test
  (is (= 7 (add-any 3 4)))
  (is (= "aabbb" (add-any "aa" "bbb")))
  (is (= [:a 3 4] (add-any [:a 3] [4]))))

(spec/def ::doubles (spec/* double?))

(defspecfun malformed-add
  (::doubles [x] (apply + x))
  (::numbers [x] (apply + x)))

(defn threw [f]
  (try 
    (f)
    false
    (catch Throwable _
      true)))

(deftest malformed-test
  (is (= 9 (malformed-add 6 3)))
  (is (threw #(malformed-add 9.0 3.0)))
  (is (threw #(malformed-add [:a] [3 4]))))
