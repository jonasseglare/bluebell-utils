(ns bluebell.utils.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.test.alpha :as stest]
            [bluebell.utils.traverse :as traverse]
            [bluebell.utils.core :refer :all]))

(stest/instrument)

(deftest map-with-keys-test
  (testing "Testing map with keys"
    (is (map-with-keys? {:rulle 119} #{:rulle}))
    (is (map-with-keys? {:rulle 119} #{:rulle}))
    (is (not (map-with-keys? {:rulle 119} #{:rull})))
    (is (not (map-with-keys? [:rulle] #{:rulle})))))

(deftest matrix-index-test
  (testing "Matrix index computation"
    (is (= 0 (compute-matrix-index [3 4] [0 0])))
    (is (= 1 (compute-matrix-index [3 4] [1 0])))
    (is (= 3 (compute-matrix-index [3 4] [0 1])))
    (is (= 4 (compute-matrix-index [3 4] [1 1])))))

(deftest provide-arguments-test
  (testing "Providing arguments"
    (let [f (provide-arguments get [[{:a 3 :b 4}] nil])]
      (is (= 3 (f :a)))
      (is (= 4 (f :b))))
    (let [f (provide-arguments get [nil [:a]])]
      (is (= 9 (f {:a 9})))
      (is (= 12 (f {:a 12}))))))

(deftest map-hierarchy-test
  (testing "Flattening..."
    (is (= (flatten-map-hierarchy {:a {1 :c 2 :d} :b {3 :k}})
           {[:a 1] :c, [:a 2] :d, [:b 3] :k}))))

(deftest group-transducer-test
  (is (reduce ((bundle 2) conj) [] [1 2 3 4 5 6])
      [[1 2] [3 4] [5 6]]))

(defn count-and-inc [state x]
  [(inc state) (inc x)])

;; TODO: unit test elsewhere.
(deftest traverse-postorder
  (is (= (traverse/traverse-postorder-cached [1 2 3] {:visit (only-visit number? inc)})
         [2 3 4]))
  (is (= (traverse/traverse-postorder-cached
          {} [1 1 1 3]
          {:visit (only-visit number? inc)})
         [{1 {:mapped 2, :parents {[1 1 1 3] 3}},
           3 {:mapped 4, :parents {[1 1 1 3] 1}},
           [1 1 1 3]
           {:mapped [2 2 2 4], :parents #:bluebell.utils.traverse{:parent 1}}}
          [2 2 2 4]]
         )))

(deftest traverse-with-state
  (is (= [7 [["0" "3" "4"] "3" "4"]]
         (traverse/traverse-postorder-with-state
          0 
          [[0 3 4] 3 4] 
          {:visit (fn [state x]
                    [(inc state) (if (coll? x) x (str x))])
           })))
  (is (= (traverse/traverse-postorder-with-state
                      0 {:a 3 :b 3}
                      {:visit (fn [state x]
                                (if (number? x)
                                  [(inc state) (str x)]
                                  [state x]))})
         [2 {:a "3", :b "3"}])))


(deftest traverse-cached-with-processing
  (let [top [3 {:a [9 [2 2 2]]}
             [2 2 2] ]
        m (first
           (traverse/traverse-postorder-cached {} top
                                      {:visit identity}))]
    (is (= {[2 2 2] 2}
           (-> (get (traverse/register-child-at m [2 2 2] [2 2 2] 1) top)
               :children)))
    (is (= 6 (get (:children (get (traverse/register-children m) top)) 2)))))
                 
(deftest with-value-test
  (is (= {:a 9 :b 4}
         (with-value [a {}]
           (assoc a :a 9)
           (assoc a :b 3)
           (update a :b inc))))
  (is (= (with-value [a (range 12)]
           (filter odd? a)
           (map inc a)
           (map #(/ % 2) a))
         (range 1 7))))

(deftest subexpr-test
  (let [m (traverse/compute-subexpressions {:a [1 2] :b [{:d [{:c 3}]}]})]
    (is (contains? (get m :a) :a))
    (is (not (contains? (get m :a) 1)))
    (is (contains? (get m [1 2]) 2))
    (is (not (contains? (get m [1 2]) {:b 3})))
    (is (contains?
         (get m {:a [1 2] :b [{:d [{:c 3}]}]})
         {:c 3}))))

(deftest indentation-test
  (is (= (indent-nested {:prefix "\n" :step "  "}
                        ["a" "b" ["c" [{:prefix "\n--->"} "kattskit" ["bra va?"]]]])
         "\na\nb\n  c\n--->kattskit\n--->  bra va?")))
