(ns bluebell.utils.error-context-test
  (:require [bluebell.utils.error-context :refer :all]
            [clojure.test :refer :all]))

(deftest error-value-test
  (is (error? (ex-info "Mjao" {})))
  (is (not (error? "Mjao"))))

(deftest context-test
  (is (not (context? 9)))
  (let [c (context)]
    (is (context? c))
    (is (ok? c)))
  (is (= 7 (call (context) + 3 4))))

(deftest exceptions
  (let [c (catch-ex (context) ArithmeticException)]
    (is (nil? (call c / 3 0))))
  (let [c (context)]
    (is (thrown? ArithmeticException (call c / 3 0))))
  (let [c (-> (context)
              (catch-ex ArithmeticException)
              (catch-ex clojure.lang.ExceptionInfo))]
    
    (is (nil? (call c / 3 0))))
  (let [c (catch-ex (context) Exception)]
    (is (= 7 (call c + 3 (call c / 8 2))))
    (is (nil? (call c + 3 (call c / 8 0))))
    (is (instance? ArithmeticException
                   (get-error c)))))

(deftest error-mapping
  (let [c (-> (context)
              (catch-ex ArithmeticException)
              (map-error (constantly :bad)))]
    (is (= 7 (call c + 3 4)))
    (is (ok? c))
    (is (nil? (get-error c)))
    (is (nil? (call c / 8 0)))
    (is (= :bad (get-error c)))))

(deftest partial-test
  (let [c (-> (context)
              (catch-ex ArithmeticException))
        safediv (partial call c /)]
    (is (= 9 (safediv 18 2)))
    (is (nil? (safediv 3 0)))
    (is (not (ok? c)))))

(deftest basic-error-test
  (let [e (error-value :mjao "Mjao!!!")]
    (is (error? e))
    (is (= :mjao (error-key e)))
    (is (= "Mjao!!!" (message e)))))

(defn my-div [a b]
  (if (= 0 b)
    (error-value :div-by-0 "Division by zero")
    (/ a b)))

(deftest error-value-test
  (is (error? (my-div 3 0)))
  (is (= 7 (my-div 14 2)))
  (let [c (context)]
    (is (= 14 (call c my-div 28 2)))
    (is (= 119 (export c 119)))
    (is (nil? (call c my-div 9 0)))
    (is (error? (export c 7)))))
