(ns bluebell.utils.wip.async-test
  (:require [clojure.test :refer :all]
            [bluebell.utils.wip.async :refer :all]
            [clojure.core.async :as async]))

(defn inc-up-to-5 [x]
  (when (< x 4)
    (inc x)))

(deftest producer-and-exhaust-test
  (is (= [0 1 2 3 4] (exhaust (producer
                               inc-up-to-5
                               0
                               (async/chan))))))

(defn mul-by-2 [x]
  (* 2 x))

(deftest with-transducer-test
  (is (= [0 2 4 6 8] (exhaust (producer
                               inc-up-to-5
                               0 (async/chan 
                                  (async/buffer 1) 
                                  (map mul-by-2)))))))
