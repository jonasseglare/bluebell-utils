(ns bluebell.utils.wip.check-test
  (:require [bluebell.utils.wip.check :refer :all :as check]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Function IO validation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def validate-a? true)
(def validate-b? false)

(deftest check-fn-io-test
  (is (spec/valid? ::check/check-io-args [[]]))
  (is (spec/valid? ::check/check-io-args [[:pre []]]))
  (is (spec/valid? ::check/check-io-args '[[:pre [(number? a)]]]))
  (is (spec/valid? ::check/check-io-args '[[:pre [(number? a)]
                                            :post k []]]))
  (is (spec/valid? ::check/check-io-args '[[:pre [(number? a)]
                                            :post k [(number? k)]]]))

  (is (spec/valid? ::check/check-io-args '[[:pre [(number? a)]
                                            :post k [(number? k)]]
                                           :some-body-goes-here]))
  (is (spec/valid? ::check/check-io-args '[[:when kattskit
                                            :pre [(number? a)]
                                            :post k [(number? k)]]
                                           :some-body-goes-here]))
  
  (is (spec/valid?
       ::check/check-io-args
       '[
         
         [
          :pre
          [(number? b)]
          :post x
          [(number? x)]


          ]


         ]))
  (is (spec/valid?
       ::check/check-io-args
       '[[:when k
          :pre
          [(number? b)]
          :post x
          [(number? x)]
          ]
         
         :mjao 119]))
  (let [code (#'check/generate-check [:s-expr '(number? :a)])]
    (is (thrown? Throwable (eval code))))
  (let [code (#'check/generate-check [:s-expr '(number? 9)])]
    (is (nil? (eval code))))
  (is (= 17
         (let [a 14]
           (check-io [:pre [(number? a)]
                      :post k [(number? k)]]
                     (+ a 3)))))
  (is (thrown? Throwable  (let [a :a]
                            (check-io [:pre [(number? a)]
                                       :post k [(number? k)]]
                                      (+ a 3)))))
  (is (thrown? Throwable  (let [a 119]
                            (check-io [:pre [(number? a)]
                                       :post k [(number? k)]]
                                      :kattskit))))
  (is (= :a (check-io [:post k []] :a)))
  (is (= :a (check-io [:when true] :a)))
  (is (= :a (check-io [] :a)))
  (is (= :a (check-io [:post k [(keyword? k)]] :a)))
  (is (thrown? Throwable (check-io [:post k [(keyword? k)]] 119)))
  (is (= 119 (check-io [:when validate-b?
                        :post k [(keyword? k)]] 119)))
  (is (thrown? Throwable (check-io [:when validate-a?
                                    :post k [(keyword? k)]] 119)))
  (is (= [3 4] (check-io [:post [a b] [(number? a)
                                       (number? b)]]
                         [3 4]))))



(deftest checked-defn-test
  (is (spec/valid? ::check/checked-defn-args
                   '(katt [::a a] 1 2 3)))
  (is (spec/valid? ::check/checked-defn-args
                   '(katt [::a a
                           ::b b] 1 2 3)))
  (is (spec/valid? ::check/checked-defn-args
                   '(katt [:when 9
                           ::a a
                           ::b b] 1 2 3)))
  (is (not (spec/valid? ::check/checked-defn-args
                        '(katt [:when 9 :when
                                ::a a
                                ::b b] 1 2 3))))
  (is (spec/valid? ::check/checked-defn-args
                   '(katt [:when 9
                           ::a a
                           ::b b
                           :post k []] 1 2 3))))

(checked-defn kattskit2 [:when true
                         
                         number? a
                         number? b
                         
                         :post k [(number? k)]]
              (+ a b))

(deftest kattskit2-test
  (is (= 7 (kattskit2 3 4))))
