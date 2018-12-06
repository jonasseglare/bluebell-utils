(ns bluebell.utils.ReadAndUpdateMachine-test
  (:import [bluebell.utils
            ReadAndUpdateMachine
            ReadAndUpdateMachineSettings])
  (:require [clojure.test :refer :all]))

(deftest raum-test
  (let [m (ReadAndUpdateMachine.)]
    (is (= 0 (.getState m)))
    (.beginRead m)
    (is (= 1 (.getState m)))
    (.endRead m)
    (is (= 0 (.getState m)))
    (.beginRead m)
    (.beginRead m)
    (.beginRead m)
    (is (= 3 (.getState m)))
    (.endRead m)
    (.endRead m)
    (.endRead m)
    (is (= 0 (.getState m)))
    (.beginUpdate m)
    (is (= -1 (.getState m)))
    (.beginUpdate m)
    (.beginUpdate m)
    (.beginUpdate m)
    (is (= -4 (.getState m)))
    (.endUpdate m)
    (.endUpdate m)
    (.endUpdate m)
    (is (= -1 (.getState m)))
    (.endUpdate m)
    (is (= 0 (.getState m)))
    (.beginUpdate m)
    (.beginUpdate m)
    (is (= -2 (.getState m)))
    (.beginRead m)
    (is (= -3 (.getState m)))
    (.endRead m)
    (is (= -2 (.getState m)))
    (.endUpdate m)
    (.endUpdate m)
    (is (.tryBeginUpdate m))
    (is (= -1 (.getState m)))
    (.endUpdate m)
    (.beginRead m)
    (is (= 1 (.getState m)))
    (is (not (.tryBeginUpdate m)))
    (is (= 1 (.getState m)))
    (.endRead m)
    (is (= 0 (.getState m)))))

(deftest wrong-end-0
  (let [m (ReadAndUpdateMachine.)]
    (is (thrown? Exception (.endRead m)))))

(deftest wrong-end-1
  (let [m (ReadAndUpdateMachine.)]
    (is (thrown? Exception (.endUpdate m)))))

(deftest update-while-read-test
  (let [settings (ReadAndUpdateMachineSettings.)
        _ (set! (.debug settings) true)
        m (ReadAndUpdateMachine. settings)]
    (.beginRead m)
    (is (thrown? Exception (.beginUpdate m)))))

(def test-data (range 100))

(deftest concurrent-test
  (let [m (ReadAndUpdateMachine.)
        arr (int-array [0 1])
        provider (future
                   (doseq [a test-data]
                     (.beginUpdate m)
                     (aset arr 0 a)
                     (aset arr 1 (inc a))
                     (.endUpdate m)))]
    (dotimes [i (count test-data)]
      (.beginRead m)
      (is (= (inc (aget arr 0))
             (aget arr 1)))
      (.endRead m))))
