(ns bluebell.utils.symset-test
  (:require [clojure.test :refer :all]
            [bluebell.utils.specutils :as sutils]
            [bluebell.utils.symset :refer :all :as ss])
  (:refer-clojure :exclude [complement any?]))

(deftest basic-tests
  (is (set-registry? empty-set-registry))
  (let [a (sutils/force-conform
                ::ss/registry
                (member-of empty-set-registry :x :numbers))
        _ (is (= #{:numbers} (set-memberships a :x)))
        _ (is (empty? (set-memberships a :y)))
        b (member-of a :x :elements)
        _ (is (= #{:numbers :elements} (set-memberships b :x)))
        _ (is (= #{:numbers :elements} (all-sets b)))
        c (subset-of b :rationals :numbers)
        _ (is (= #{:numbers :elements :rationals} (all-sets c)))
        _ (is (member-of? c :x :numbers))
        _ (is (not (member-of? c :x :rationals)))
        d (member-of c :y :rationals)
        _ (is (= #{:numbers :rationals} (set-memberships d :y)))
        _ (is (= #{:numbers :elements} (set-memberships d :x)))
        _ (is (= #{:x :y} (evaluate-query d :numbers)))
        _ (is (= #{:x} (evaluate-query d (complement :rationals))))
        e (member-of d :z :elements)
        _ (is (= #{:x}
                 (evaluate-query e (intersection :numbers :elements))))
        _ (is (= #{:y}
                 (evaluate-query e (difference :numbers :elements))))
        _ (is (= #{:x :y :z}
                 (evaluate-query e (union :numbers :elements))))

        _ (is (= #{:x :y :z}
                 (all-elements e)))

        _ (is (= #{}
                 (evaluate-query e #{:kattskit})))
        
        _ (is (= #{:kattskit}
                 (evaluate-query (add-element e :kattskit) #{:kattskit})))
        _ (is (= #{:x :y :z}
                 (evaluate-query e universe)))
        ]
    ))

(defn vec-gen [x]
  (if (vector? x)
    #{{:vec (count x)}} 
    #{}))

(defn map-gen [x]
  (if (map? x)
    #{:map} 
    #{}))

(deftest generator-test
  (let [s empty-set-registry
        s (add-superset-generator s :map-gen map-gen)
        s (add-set s [:num :num :num])
        s (add-superset-generator s :vec-gen vec-gen)
        _ (is (= (supersets-of s #{[:num :num :num]})
                 #{[:num :num :num]
                   {:vec 3}
                   :map}))]
    ))
