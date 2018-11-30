(ns bluebell.utils.ConcurrentStateMachine-test
  (:import [bluebell.utils ConcurrentStateMachine
            IStateTransition])
  (:require [clojure.test :refer :all]))


(defn transition [tr-fn]
  (proxy [IStateTransition] []
    (nextOrNull [current-state]
      (tr-fn current-state))))

(def next-number (transition (fn [x]
                               (if (number? x) (inc x)))))

(def previous-number (transition (fn [x]
                                   (if (number? x) (dec x)))))

(def begin-update (transition (fn [x] (if (= 0 x) :updating))))
(def end-update (transition (fn [x] (if (= :updating x) 0))))

(deftest easy-test
  (let [sm (ConcurrentStateMachine. 0)]
    (is (= 1 (.take (.submit sm next-number))))
    (is (= 2 (.perform sm next-number)))
    (is (= 1 (.take (.submit sm previous-number))))
    (is (= 0 (.take (.submit sm previous-number))))
    (is (= :updating (.take (.submit sm begin-update))))
    (is (= 0 (.take (.submit sm end-update))))
    (is (= :updating (.take (.submit sm begin-update))))
    (is (= 0 (.getPendingCount sm)))
    (let [p (.submit sm next-number)]
      (is (= 1 (.getPendingCount sm)))
      (is (= :updating (.getCurrentState sm)))
      (let [p2 (.submit sm next-number)]
        (is (= 2 (.getPendingCount sm)))
        (is (= 0 (.take (.submit sm end-update))))
        (is (= 2 (.getCurrentState sm)))
        (is (= 1 (.take p)))
        (is (= 2 (.take p2)))
        (is (= 0 (.getPendingCount sm)))))))
