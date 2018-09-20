(ns bluebell.utils.dsl-test
  (:require [bluebell.utils.dsl :refer :all]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]))

(deftest settings-test
  (is (= 3 ((get-import-fn {}) 3)))
  (is (= 3 ((get-accumulator-validator {}) 3)))
  (is (= 3 ((get-context-validator {}) 3)))
  (is (thrown? Exception ((get-accumulator-validator {:accumulator-spec number?}) :a)))
  (is (= 3 ((get-accumulator-validator {:accumulator-spec number?}) 3)))
  (is (= 3 ((get-context-validator {:context-spec number?}) 3)))
  (is (thrown? Exception ((get-context-validator {:context-spec number?}) :a))))

(spec/def ::inc number?)
(spec/def ::sum number?)
(spec/def ::my-context (spec/keys :req-un [::inc]))
(spec/def ::my-accumulator (spec/keys :req-un [::sum]))

(defn import-fn [x]
  (if (number? x)
    (fn [c a]
      (update a :sum + (* x (:inc c))))
    x))

(def settings {:accumulator-spec ::my-accumulator
               :context-spec ::my-context
               :import-fn import-fn})

(def eval-dsl (dsl-evaluator settings))

(def empty-context {:inc 1})
(def empty-accumulator {:sum 0})

(defn with-increment [i & body]
  (fn [c a]
    (eval-dsl (assoc c :inc i) a body)))

(deftest mini-dsl-test
  (is (= (eval-dsl empty-context empty-accumulator 1)
         {:sum 1}))
  (is (= (eval-dsl empty-context empty-accumulator 1 2 3 4)
         {:sum 10}))
  (is (= (eval-dsl empty-context empty-accumulator 1 [2 [[[3]]] 4])
         {:sum 10}))
  (is (= (eval-dsl empty-context empty-accumulator 1 2 3 (with-increment 100
                                                           5) 30)
         {:sum 536}))
  (is (thrown? Exception (eval-dsl empty-context empty-accumulator 1 2 :b (with-increment 100
                                                           5) 30))))
