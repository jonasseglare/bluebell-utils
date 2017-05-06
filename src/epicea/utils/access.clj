(ns epicea.utils.access
  (:require [epicea.utils.debug :as dbg]
            [epicea.utils.optional :refer [optional]]))

(defn accessor? [x]
  (and (map? x)
       (every? #(contains? x %) [:set :get :has?])))

;;;;; Standard accessors

(def default-obj {})
(def default-opts {})
(def default-map-opts (merge default-opts {:default-parent {}}))

;; Assumptions:
;;  - Always a valid input object.

(defn map-accessor 
  ([key map-opts]
   (merge default-map-opts map-opts
          {:get key
           :valid-parent? map?
           :has? #(contains? % key)
           :set (fn [obj x] (assoc obj key x))}))
  ([key] (map-accessor key {})))

(def default-vector-opts default-opts)

(defn vector-accessor 
  ([index opts]
   (merge default-vector-opts 
          opts
          (let [valid? #(and (vector? %) (< index (count %)))]
            (assert (valid? (:default-parent opts)))
            {:get #(nth % index)
             :valid-parent? valid?
             :has? valid?
             :set (fn [obj x] 
                    (assert (valid? obj))
                    (assoc obj index x))}))))

;; Wraps an element in a vector
(def vec1-accessor (merge default-opts {:get (fn [x] [x]) 
                                        :set (fn [x y] (first y))
                                        :has? (constantly true)}))
          
;;;;; Utilities
(defn getx [accessor obj]
  ((:get accessor) obj))

(defn setx [accessor obj x]
  ((:set accessor) obj x))

(defn updatex [accessor obj f]
  (if ((:has? accessor) obj)
    ((:set accessor) obj (f ((:get accessor) obj)))
    obj))

(defn has? [accessor obj]
  ((:has? accessor) obj))

(defn getx-or-default [accessor obj]
  (cond 
    ((:has? accessor) obj) ((:get accessor) obj)
    (contains? accessor :make-default) ((:make-default accessor) obj)
    :default ((:get accessor) (:default-parent accessor))))

(defn getx-optional [accessor obj]
  (if ((:has? accessor) obj)
    (optional ((:get accessor) obj))
    (optional)))


(defn er [which]
  (fn [accessor]
    (assert (accessor? accessor))
    (get accessor which)))

(def hasser? (er :has?))

(def getter (er :get))

(def setter (er :set))

(defn updater [accessor]
  (fn [obj f] (updatex accessor obj f)))

(defn parts [accessor]
  (if (contains? accessor :parts)
    (:parts accessor) 
    [accessor]))

(defn complex? [accessor]
  (contains? accessor :parts))

(defn simple? [accessor]
  (not (simple? accessor)))

(defn size [x]
  (count (parts x)))

;;;;; Composition

(defn compose-getx [a b]
  (fn [x] (-> x a b)))

(defn compose-has? [a b]
  (fn [x] (and ((:has? a) x) 
               ((:has? b) ((:get a) x)))))

(defn compose-setx [a b]
  (fn [obj x] 
    (setx a obj (setx b (getx-or-default a obj) x))))

(defn catparts [a b]
  (vec (concat a b)))

(defn compose2 [a b]
  {:parts (catparts (parts a) (parts b))
   :default-parent (:default-parent a)
   :make-default (:make-default b)
   :get (compose-getx (:get a) (:get b))
   :has? (compose-has? a b)
   :set (compose-setx a b)})

(defn compose [& args]
  (reduce compose2 args))

;;;;; Slicing, etc
(defn slice [accessor from to]
  (reduce compose (subvec (parts accessor) from to)))

(defn diff-ancestors [a b]
  (loop [result []
         ap (parts a)
         bp (parts b)]
    (if (or (empty? ap) 
            (empty? bp)
            (not= (first ap) (first bp)))
      {:common result
       :a ap
       :b bp}
      (recur (conj result (first ap))
             (rest ap)
             (rest bp)))))

;;;;;; Constructor

(defn constructor 
  ([init accessors]
   (fn [& args]
     (reduce
      (fn [obj f]
        (f obj))
      init
      (map (fn [accessor arg]
             (fn [obj] 
               ((:set accessor) 
                obj arg)))
           accessors args))))
  ([accessors]
   (constructor (:default-parent (first accessors))
                accessors)))


      
            
            
