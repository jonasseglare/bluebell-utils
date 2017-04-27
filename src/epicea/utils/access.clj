(ns epicea.utils.access
  (:require [epicea.utils.debug :as dbg]
            [epicea.utils.optional :refer [optional]]))

(defn accessor? [x]
  (and (map? x)
       (every? #(contains? x %) [:id :default-parent :set :get :has?])
       (or (contains? x :id)
           (contains? x :parts))))

;;;;; Standard accessors
(defn map-accessor 
  ([key dp]
   {:id [::map-accessor key dp]
    :default-parent dp
    :get key
    :has? #(contains? % key)
    :set (fn [obj x] (assoc obj key x))})
  ([key] (map-accessor key {})))

(defn vector-accessor 
  ([index dp]
   (let [valid? #(and (vector? %)
                      (< index (count %)))]
     (assert (valid? dp))
     {:id [::vector-accessor index dp]
      :default-parent dp
      :get #(nth % index)
      :has? valid?
      :set (fn [obj x] 
             (assoc (if (valid? obj) obj dp) index x))})))
          
;;;;; Utilities
(defn getx [accessor obj]
  ((:get accessor) obj))

(defn setx [accessor obj x]
  ((:set accessor) obj x))

(defn updatex [accessor obj f]
  ((:set accessor) obj (f ((:get accessor) obj))))

(defn has? [accessor obj]
  ((:has? accessor) obj))

(defn getx-or-default [accessor obj]
  ((:get accessor) 
   (if ((:has? accessor) obj)
     obj
     (:default-parent obj))))

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

(defn id [accessor]
  (if (contains? accessor :id) 
    (:id accessor)
    (map id (:parts accessor))))

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

(defn compose [a b]
  {:parts (catparts (parts a) (parts b))
   :default-parent (:default-parent a)
   :get (compose-getx (:get a) (:get b))
   :has? (compose-has? a b)
   :set (compose-setx a b)})


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


      
            
            
