(ns bluebell.utils.ebo-test
  (:require [bluebell.utils.ebo :refer :all :as ebo]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]))

(spec/def ::mjao (partial = [:mjao 119]))

(deftest spec-test
  (is (spec/valid?
       ::ebo/arg-spec
       (normalize-arg-spec {:pred number?
                            :key :number
                            :pos [] :neg[]})))

  (let [k (#'ebo/decorate-key-and-pred-from-spec {:spec ::mjao})]
    (is (contains? k :key))
    (is (contains? k :pred))
    (is ((:pred k) [:mjao 119]))
    (is (not ((:pred k) [:mjao 118]))))
  (let [k (normalize-arg-spec {:spec ::mjao
                               :pos [[:mjao 119]]
                               :neg [9 :a {} {:a 3}]})]
    (is (:valid? k)))
  (let [k (normalize-arg-spec {:spec ::mjao
                               :pos [[:mjao 119] 4]
                               :neg [9 :a {} {:a 3}]})]
    (is (not (:valid? k))))
  (let [k (normalize-arg-spec {:spec ::mjao
                               :pos [[:mjao 119]]
                               :neg [9 :a {} {:a 3} [:mjao 119]]})]
    (is (not (:valid? k)))
    (is (thrown? Exception (check-valid k)))
    (is (= k (normalize-arg-spec k)))))

(def-arg-spec mummi {:pred number?
                     :desc "Any number"
                     :pos [9 3 1 -3 3/4]
                     :neg []})
