(ns nettle.utils.mapdater-test
  (:require [clojure.test :refer :all]
            [nettle.utils.mapdater :refer :all :as md]
            [clojure.spec :as spec]))


(def k (mapdater a [b c] (+ b c)))
(def q (mapdater [b c] {:a (+ b c)}))
(def r (mapdater z [b c :as 9] (+ b c)))

(deftest mapdater-tests
  (is (= {:a 7 :b 3 :c 4} (k {:b 3 :c 4})))
  (is (= {:a 7 :b 3 :c 4} (q {:b 3 :c 4})))
  (is (= {:z 7 :b 3 9 4} (r {:b 3 9 4})))
  (is (= {:b 3 :d 4 :c 1007} ((mapdate c (+ :b :d 1000)) {:b 3 :d 4}))))
