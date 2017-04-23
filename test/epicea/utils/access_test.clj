(ns epicea.utils.access-test
  (:require [clojure.test :refer :all]
            [epicea.utils.access :refer :all]))

(def k (map-accessor :k))
(def w (map-accessor :w))

(def kw (compose k w))

(def set-k (setter k))
(def update-k (updater k))
(def update-kw (updater kw))

(deftest access-test
  (is (accessor? (map-accessor :k)))
  (is (not (accessor? {})))
  (is (= {:k 9} (setx k {} 9)))
  (is (not (has? k {})))
  (is (has? k (setx k {} 9))))

(deftest compose-test
  (is (has? kw {:k {:w 3}}))
  (is (= 3 (getx kw {:k {:w 3}})))
  (is (= {:r 3 
          :k {:w 4}} (setx kw {:r 3} 4)))
  (is (= {:k 4} (update-k {:k 3} inc)))
  (is (= {:k {:w 4}} (update-kw {:k {:w 3}} inc))))
