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
  (is (= {:k {:w 4}} (update-kw {:k {:w 3}} inc)))
  (is (= 2 (size kw)))
  (is (= 1 (size k)))
  (is (= (id w) (id (slice kw 1 2))))
  (is (not (empty? (:common (diff-ancestors kw k)))))
  (is (empty? (:common (diff-ancestors kw w)))))

(def sec (vector-accessor 1 [nil nil nil nil]))

(deftest vector-test
  (is (= [nil 3 nil nil] (setx sec nil 3)))
  (is (= [nil 4 nil nil] (updatex sec [nil 3 nil nil] inc))))

(def x (map-accessor :x))
(def y (map-accessor :y))
(def make-xy (constructor {} [x y]))

(deftest constructor-test
  (is (= {:x 3 :y 4} (make-xy 3 4))))


(def empty-xy2 [nil nil])
(def x2 (vector-accessor 0 empty-xy2))
(def y2 (vector-accessor 1 empty-xy2))
(def make-xy2 (constructor [x2 y2]))

(deftest constructor-test2
  (is (= [9 20] (make-xy2 9 20))))
