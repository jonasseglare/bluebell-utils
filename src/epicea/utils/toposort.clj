(ns epicea.utils.toposort
  (:require [epicea.utils.debug :as debug]))

(def test-map {:a [:b :c]
               :b [:c]})

(defn insert-predecessors [m [key successors]]
  (reduce
   (fn [m successor]
     (if (contains? m successor)
       (update-in m [successor] #(conj % key))
       (assoc m successor [key])))
   m
   successors))
       
(defn make-predecessor-map [successor-map]
  (reduce 
   insert-predecessors
   {}
   successor-map))

(defn nodes-with-no-incoming-edges [successor-map pred-map]
  (clojure.set/difference (set (keys successor-map))
                          (set (keys pred-map))))

(defn exhaust [x]
  (last (take-while (complement nil?) x)))

(defn remove-map-list-item [m k v]
  (update-in m [k] #(disj (set %) v)))

(defn visit-successor [[root-nodes suc-map pred-map result] successor]
  (let [n (last result)
        new-preds (remove-map-list-item pred-map successor n)]
    [(if (empty? (get new-preds successor))
       (conj root-nodes successor)
       root-nodes) 
     (remove-map-list-item suc-map n successor)
     new-preds
     result]))

(defn toposort-iteration [[root-nodes suc-map pred-map result]]
  (println "TOPOSORT ITERATION")
  (debug/dout root-nodes)
  (debug/dout pred-map)
  (debug/dout result)
  (when (not (empty? root-nodes))
    (let [n (first root-nodes)
          successors (get suc-map n)]
      (reduce visit-successor [(rest root-nodes)
                               suc-map
                               pred-map
                               (conj result n)]
              successors))))

(defn toposort [successor-map]
  (let [pred-map (make-predecessor-map successor-map)
        start (nodes-with-no-incoming-edges successor-map pred-map)]
    (exhaust (iterate toposort-iteration 
                      [start
                       successor-map
                       pred-map
                       []]))))

