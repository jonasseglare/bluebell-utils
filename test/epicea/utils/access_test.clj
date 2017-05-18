(ns epicea.utils.access-test
  (:require [clojure.test :refer :all]
            [epicea.utils.access :refer :all]
            [epicea.utils.optional :refer [optional]]))

(def k (map-accessor :k))
(def w (map-accessor :w))

(def kw (compose k w))

(def set-k (setter k))
(def get-k (getter k))
(def update-k (updater k))
(def update-kw (updater kw))

(deftest access-test
  (is (accessor? (map-accessor :k)))
  (is (not (accessor? {})))
  (is (= {:k 9} (setx {} k 9)))
  (is (not (has? {} k)))
  (is (has? (setx {} k 9) k))
  (is (= 9 (getx {:k 9} k)))
  (is (= 9 (get-k {:k 9}))))

(deftest compose-test
  (is (has? {:k {:w 3}} kw))
  (is (= 3 (getx {:k {:w 3}} kw)))
  (is (= {:r 3 :k {:w 4}} (setx {:r 3} kw 4)))

  (is (= {:k 4} (update-k {:k 3} inc)))
  (is (= {:k {:w 4}} (update-kw {:k {:w 3}} inc)))
  (is (= 2 (size kw)))
  (is (= 1 (size k)))
  (is (not (empty? (:common (diff-ancestors kw k)))))
  (is (empty? (:common (diff-ancestors kw w)))))

(def default-vec [nil nil nil nil])
(def sec (vector-accessor 1 {:default-parent default-vec}))


(deftest vector-test
  (is (= [nil 3 nil nil] (setx default-vec sec 3)))
  (is (= [nil 4 nil nil] (updatex [nil 3 nil nil] sec inc))))

(comment
  (def x (map-accessor :x))
(def y (map-accessor :y))
(def make-xy (constructor {} [x y]))

(deftest constructor-test
  (is (= {:x 3 :y 4} (make-xy 3 4))))


(def empty-xy2 [nil nil])
(def x2 (vector-accessor 0 {:default-parent empty-xy2}))
(def y2 (vector-accessor 1 {:default-parent empty-xy2}))
(def make-xy2 (constructor [x2 y2]))

(deftest constructor-test2
  (is (= [9 20] (make-xy2 9 20))))

(def even-access
  {:id :even-number
   :default-parent 0
   :get identity
   :has? even?
   :set (fn [a b] b)})

(deftest even-access-test
  (is (= 2 (getx even-access 2)))
  (is (= (optional 2) (getx-optional even-access 2)))
  (is (= (optional) (getx-optional even-access 3))))

(def whiskers2 (map-accessor :whiskers {:default-parent {:type :cat :whiskers []}}))
(def whiskers3 (map-accessor :whiskers {:make-default (fn [_] [1 2 3])}))

(deftest cat-test
  (is (= [] (getx-or-default whiskers2 {})))
  (is (= [1 2 3] (getx-or-default whiskers3 {}))))

(deftest vec1-accessor-test
  (is (= 4 (updatex vec1-accessor 3 (fn [k] (map inc k))))))
)
