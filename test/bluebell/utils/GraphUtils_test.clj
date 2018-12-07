(ns bluebell.utils.GraphUtils-test
  (:import [bluebell.utils GraphUtils INeighborhoodFunction]
           [java.util List])
  (:require [clojure.test :refer :all]))


(defn map-neigh [m]
  (proxy [INeighborhoodFunction] []
    (getNeighbors [src]
      (get m src))))


;; (def n012 ))

(deftest basic-test
  (is (not (empty? (GraphUtils/findCycles
                    [0]
                    (map-neigh {0 [1] 1 [2] 2 [0]})))))
  (is (empty? (GraphUtils/findCycles
               [0]
               (map-neigh {0 [1] 1 [2] 2 []}))))
  (is (not (empty? (GraphUtils/findCycles
                    [0]
                    (map-neigh {0 [1] 1 [0]}))))))
