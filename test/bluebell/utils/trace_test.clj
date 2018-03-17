(ns bluebell.utils.trace-test
  (:require [bluebell.utils.trace :refer :all :as trace]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]))

(deftest basic-trace
  (let [f (trace-fn)]
    (record f :a)
    (record f :b)
    (record f :c)
    (is (= (map first (f))
           [:a :b :c]))))

(def sample-trace-3
  (let [f (trace-fn)]
    (record f :a)
    (begin f :b)
    (begin f :c)
    (record f :kattskit)
    (end f :c)
    (end f :b)
    (record f :d)
    (f)))

(deftest begin-end-test
  (is (spec/valid? ::trace/trace sample-trace-3))
  (is (empty? (-> sample-trace-3 parse-spec list-invalid-blocks)))
  (is (empty? (-> (let [f (trace-fn)]
                            (begin f :a)
                            (end f :a)
                            (f))
                          parse-spec
                          list-invalid-blocks)))
  (is (not (empty? (-> (let [f (trace-fn)]
                         (begin f :a)
                         (end f :b)
                         (f))
                       parse-spec
                       list-invalid-blocks)))))

(deftest parse-test-full
  (is (thrown? Throwable
               (parse (let [f (trace-fn)]
                        (begin f :a)
                        (end f :b)
                        (f)))))
  (is (parse (let [f (trace-fn)]
               (begin f :a)
               (end f :a)
               (f)))))
