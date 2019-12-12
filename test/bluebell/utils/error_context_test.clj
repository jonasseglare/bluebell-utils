(ns bluebell.utils.error-context-test
  (:require [bluebell.utils.error-context :refer :all]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as spec])
  (:refer-clojure :exclude [do]))

(deftest context-test
  (is (not (context? 9)))
  (let [c (context)]
    (is (context? c))
    (is (ok? c)))
  (is (= 7 ( (context) + 3 4))))

(deftest exceptions
  (let [c (catch-ex (context) ArithmeticException)]
    (is (nil? ( c / 3 0))))
  (let [c (context)]
    (is (thrown? Exception (c / 3 0))))
  (let [c (-> (context)
              (catch-ex ArithmeticException)
              (catch-ex clojure.lang.ExceptionInfo))]
    
    (is (nil? ( c / 3 0))))
  (let [c (catch-ex (context) Exception)]
    (is (= 7 ( c + 3 ( c / 8 2))))
    (is (nil? ( c + 3 ( c / 8 0))))
    (is (instance? ArithmeticException
                   (get-error c)))))

(deftest error-mapping
  (let [c (-> (context)
              (catch-ex ArithmeticException)
              (maperr (constantly :bad)))]
    (is (= 7 ( c + 3 4)))
    (is (ok? c))
    (is (nil? (get-error c)))
    (is (nil? ( c / 8 0)))
    (is (= :bad (get-error c)))))

(deftest partial-test
  (let [c (-> (context)
              (catch-ex ArithmeticException))
        safediv (partial c /)]
    (is (= 9 (safediv 18 2)))
    (is (nil? (safediv 3 0)))
    (is (not (ok? c)))))

(deftest basic-error-test
  (let [e (error-value :mjao "Mjao!!!")]
    (is (error-value? e))))

(defn my-div [a b]
  (if (= 0 b)
    (error-value :div-by-0 "Division by zero")
    (/ a b)))

(deftest error-value-test
  (is (error-value? (my-div 3 0)))
  (is (= 7 (my-div 14 2)))
  (let [c (context)]
    (is (= 14 ( c my-div 28 2)))
    (is (= 119 (export c 119)))
    (is (nil? ( c my-div 9 0)))
    (is (error-value? (export c 7)))))

(deftest function-syntax
  (is (= 7 ((context) + 3 4)))
  (let [c (catch-ex (context) ArithmeticException)]
    (is (= 8 (c / 16 2)))
    (is (nil? (c / 16 0)))
    (is (instance? ArithmeticException (c)))))

(spec/def ::arg (spec/cat :type keyword?
                          :amount number?))

(defn not-invalid? [x]
  (not= ::spec/invalid x))

(defn add-args [a b]
  (let [c (context)
        
        a (expect (constant-error c :bad-a)

                  not-invalid?
                  (spec/conform ::arg a))
        
        b (expect c not-invalid?
                  (spec/conform ::arg b)
                  (constantly :bad-b))
        
        _ (check (constant-error c :different-types)
                 = (:type a) (:type b))]
    
    (with-export c

               (+ (:amount a) (:amount b)))))

(deftest add-args-test
  (is (= 7 (add-args [:x 3] [:x 4])))
  (is (= :bad-a (add-args 3 [:x 4])))
  (is (= :bad-b (add-args [:x 3] [4])))
  (is (= :different-types (add-args [:x 3] [:y 4]))))

;; We can factor out the details about errors...
(defn stacked-ex-context []
  (-> (context)
      (maperr-when (partial instance? Exception)
                   (constantly :exception))
      (maperr-when (partial instance? ArithmeticException)
                   (constantly :arithmetic-exception))))



;; ..and then reuse it.
(deftest maperr-when-test
  (is (= 7 (with-export (stacked-ex-context)

                      7)))
  (is (= :exception (with-export (stacked-ex-context)

                               (throw (ex-info "Kattskit" {})))))

  (is (= :arithmetic-exception (with-export (stacked-ex-context)

                                          (/ 8 0)))))

(defn catching-context []
  (catch-ex (context) Exception))

(deftest with-with-test
  (is (= 7 (with (catching-context) (+ 3 4))))
  (is (nil? (with (catching-context) (/ 3 0))))
  (is (instance? Exception (with-export (catching-context) (/ 3 0)))))

(defn arith-ok-context []
  (-> (context)
      (ignore-when (partial instance? ArithmeticException)
                   (constantly :arith-ok))
      (catch-ex Exception)))

(defn arith-ok-context2 []
  (-> (context)
      (catch-ex Exception)
      (ignore-when (partial instance? ArithmeticException)
                   (constantly :arith-ok))))

(deftest ignore-test
  (let [c (arith-ok-context) ]
    (is (nil? (with c
                    (throw (ex-info "Mjao"
                                    {})))))
    (is (nil? (c * 7 17))))
  (let [c (arith-ok-context) ]
    (is (= :arith-ok (with c (/ 3 0))))
    (is (= 119 (c * 7 17))))
  (let [c (arith-ok-context2) ]
    (is (= :arith-ok (with c (/ 3 0))))
    (is (= 119 (c * 7 17)))))

(deftest slurp-test
  (is (nil? ((-> (context)
                 (catch-ex Exception))
             slurp
             "I don't think th1s file 3xists")))
  (is (nil? ((-> (context)
                 (catch-ex Exception)
                 (maperr (constantly :bad-slurp)))
             slurp
             "I don't think th1s file 3xists")))
  (is (= :bad-slurp
         (let [c (-> (context)
                     (catch-ex Exception)
                     (ignore-when (partial instance?
                                           clojure.lang.ExceptionInfo)))]
           (is (nil? (with-export
                       c (throw
                          (ex-info
                           "This exception is sort of innocent" {})))))
           (with-export (maperr c (constantly :bad-slurp))

             (slurp "I don't th1nk th1s file 3xists"))))))

(defn div-by-zero-as-zero []
  (-> (context)
      (catch-ex Exception)
      (ignore-when (partial instance? ArithmeticException)
                   (constantly 0))))


(deftest ignore-test-2
  (is (= 3 (let [c (div-by-zero-as-zero)]
             (c + 3 (c / 8 0)))))
  (is (= 7 (let [c (div-by-zero-as-zero)]
             (c + 3 (c / 8 2))))))

(deftest slurp-test-error-describe
  (let [r (with-export
            (-> (context)
                (catch-ex Exception)
                (maperr 
                 (describe "Slurp-error")))
            (slurp "Mjao"))]
    (is (error-value? :description r))))
