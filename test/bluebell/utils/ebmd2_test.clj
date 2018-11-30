(ns bluebell.utils.ebmd2-test
  (:import [bluebell.utils.ebmd ArgSpec])
  (:require [clojure.test :refer :all]
            [clojure.set :as cljset]
            [bluebell.utils.ebmd2 :refer :all]))

(def-arg-spec ::any {:pred any?
                     :pos [1 2 3 :a "a"]
                     :neg []})

(def-arg-spec ::number {:pred number?
                        :pos [1 3/4 3.4 -4]
                        :neg [:a {:a 3}]})

(def-arg-spec ::int {:pred int?
                     :pos [1 2 3]
                     :neg [13.3 :a []]})

(def-arg-spec ::string {:pred string?
                        :pos ["a" "asdf" ""]
                        :neg [3 13.3 :a []]})

(deftest basic-ebmd-test
  (is (instance? ArgSpec (resolve-arg-spec ::int)))
  (is (instance? ArgSpec
                 (resolve-arg-spec
                  (resolve-arg-spec ::int))))
  (is (matches-arg-spec? ::int 3))
  (is (not (matches-arg-spec? ::int :a)))
  (is (cljset/subset? #{::int} (arg-spec-keys))))

(declare-poly add)

(def-poly add [::number a
               ::number b]
  [:sum (+ a b)])

(def-poly add [::int a
               ::int b]
  (+ a b))

(def-poly add [::string a
               ::string b]
  (str a b))

(def-poly add [::any x]
  x)

(register-promotion ::string str ::int)

(deftest add-test
  (is (= 7 (add 3 4)))
  (is (= [:sum 3.4] (add 3 0.4)))
  (is (= "kattsk1t" (add "katt" "sk1t")))
  (is (= "kattsk1" (add "kattsk" 1)))
  (is (= :katt (add :katt))))

;; (poly-summary add)
;; (print-arg-spec-comparison [::int ::number ::string])
;; (print-arg-spec-comparison (poly-arg-specs add))
