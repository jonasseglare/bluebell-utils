(ns bluebell.utils.wip.pareto)

;; A pareto frontier is the set of all things that
;; are not dominated by any other element in the set

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Implementation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dominated-add [dominates? new-element acc existing-element]
  (when acc
    (cond
      (dominates? existing-element new-element) nil
      (dominates? new-element existing-element) acc
      :default (conj acc existing-element))))

(defn dominated-insert [dominates? current-frontier x]
  (if-let [next-frontier (reduce (partial dominated-add dominates? x)
                                 []
                                 current-frontier)]
    (conj next-frontier x)
    current-frontier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  High level API
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn frontier
  "Create an empty frontier"
  [dominates?-function]
  {:dominates? dominates?-function
   :elements []})

(defn insert
  "Insert a new element into the frontier"
  [front x]
  (update front
          :elements
          #(dominated-insert (:dominates? front) % x)))

(defn elements
  "Access the elements of the frontier"
  [front]
  (:elements front))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Example
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (do

    (def p (frontier (fn [[a b] [x y]] (and (< a x)
                                            (< b y)))))

    (def p (insert p [10 10]))

    (def p (insert p [20 5]))

    (def p (insert p [30 2]))

    (def p (insert p [29 1]))

    (def p (insert p [4 4]))

    (def p (insert p [5 5]))


    )
  )
