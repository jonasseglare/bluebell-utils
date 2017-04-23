(ns epicea.utils.access-test
  (:require [clojure.test :refer :all]
            [epicea.utils.access :refer :all]))

(def k (map-accessor :k))
(def w (map-accessor :w))

(def kw (compose k w))

(deftest access-test
  (is (accessor? (map-accessor :k)))
  (is (not (accessor? {})))
  (is (= {:k 9} (setx k {} 9)))
  (is (not (has? k {})))
  (is (has? k (setx k {} 9))))

(deftest compose-test
  (is (has? kw {:k {:w 3}}))
  (is (= 3 (getx kw {:k {:w 3}}))))
