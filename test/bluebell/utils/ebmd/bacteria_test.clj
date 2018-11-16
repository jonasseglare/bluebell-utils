(ns bluebell.utils.ebmd.bacteria-test
  (:require [bluebell.utils.ebmd :as ebmd]
            [clojure.test :refer :all]))

(defn new-bacterium [starting-position]
  {:mass 0
   :position starting-position})

(defn new-directed-bacterium [starting-position starting-directions]
  (-> (new-bacterium starting-position)
      (assoc :direction starting-directions)))

(defn bacterium? [x]
  (and (map? x)
       (contains? x :position)))

(defn directed-bacterium? [x]
  (and (bacterium? x)
       (contains? x :direction)))


(ebmd/def-arg-spec ::bacterium {:pred bacterium?
                                :pos [(new-bacterium [9 8])]
                                :neg [3 4]})

(ebmd/def-arg-spec ::directed-bacterium {:pred directed-bacterium?
                                         :pos [(new-directed-bacterium [9 8] [3 4])]
                                         :neg [(new-bacterium [3 4])]})




(ebmd/declare-poly take-turn)

(ebmd/def-poly take-turn [::bacterium x
                          ::ebmd/any-arg world]
  :aimless)

(ebmd/def-poly take-turn [::directed-bacterium x
                          ::ebmd/any-arg world]
  :directed)

(deftest take-turn-test
  (is (= :aimless (take-turn (new-bacterium [3 4]) nil)))
  (is (= :directed (take-turn (new-directed-bacterium [3 4] [1 0]) nil))))
