(ns bluebell.utils.wip.check-test
  (:require [bluebell.utils.wip.check :refer :all :as utils]
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
  (is (spec/valid? ::utils/check-io-args [[]]))
  (is (spec/valid? ::utils/check-io-args [[:pre []]]))
  (is (spec/valid? ::utils/check-io-args '[[:pre [(number? a)]]]))
  (is (spec/valid? ::utils/check-io-args '[[:pre [(number? a)]
                                            :post k []]]))
  (is (spec/valid? ::utils/check-io-args '[[:pre [(number? a)]
                                            :post k [(number? k)]]]))

  (is (spec/valid? ::utils/check-io-args '[[:pre [(number? a)]
                                            :post k [(number? k)]]
                                           :some-body-goes-here]))
  (is (spec/valid? ::utils/check-io-args '[[kattskit
                                            :pre [(number? a)]
                                            :post k [(number? k)]]
                                           :some-body-goes-here]))
  
  (is (spec/valid?
       ::utils/check-io-args
       '[
         
         [
          :pre
          [(number? b)]
          :post x
          [(number? x)]


          ]


         ]))
  (is (spec/valid?
       ::utils/check-io-args
       '[[k
          :pre
          [(number? b)]
          :post x
          [(number? x)]
          ]
         
         :mjao 119]))
  (let [code (#'utils/generate-check [:s-expr '(number? :a)])]
    (is (thrown? Throwable (eval code))))
  (let [code (#'utils/generate-check [:s-expr '(number? 9)])]
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
  (is (= :a (check-io [true] :a)))
  (is (= :a (check-io [] :a)))
  (is (= :a (check-io [:post k [(keyword? k)]] :a)))
  (is (thrown? Throwable (check-io [:post k [(keyword? k)]] 119)))
  (is (= 119 (check-io [validate-b?
                        :post k [(keyword? k)]] 119)))
  (is (thrown? Throwable (check-io [validate-a?
                                    :post k [(keyword? k)]] 119))))
