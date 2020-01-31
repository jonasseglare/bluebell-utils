(ns bluebell.utils
  (:require [clojure.spec.alpha :as spec]))

(defmacro check [& args]
  (cond
    (empty? args) (throw (ex-info "No arguments to check" {}))
    #_(= 1 (count args)) #_(let [[c] args]
                         `(if (not ~c)
                            (throw (ex-info "Check failed"
                                            {:value ~c
                                             :str ~(str c)}))))
    :default
    (let [[pred & data] args
          asyms (repeatedly (count data) gensym)]
      `(let [pred# ~pred
             ~@(reduce into [] (map vector asyms data))]
         (if (pred# ~@asyms)
           ~(first asyms)
           (throw (ex-info "Check failed"
                           {:pred-expr (quote ~pred)
                            :pred-value pred#
                            :value-expr (quote ~data)
                            :value ~(vec asyms)})))))))

(defn non-conforming-error [sp x]
  (throw (ex-info "Does not conform to spec"
                  {:spec sp
                   :value x
                   :explanation (spec/explain-str sp x)})))

(defn validate [sp x]
  (if (spec/valid? sp x)
    x
    (non-conforming-error sp x)))

(defn validate-conform [sp x]
  (let [c (spec/conform sp x)]
    (if (= c ::spec/invalid)
      (non-conforming-error sp x)
      c)))

(defn map-with-keys?
  ([ks]
   #(map-with-keys? % ks))
  ([x ks]
   (check coll? ks)
   (and (map? x)
        (every? (partial contains? x) ks))))

(defn classify
  ([classifier data]
   (classify classifier [] data))
  ([classifier empty-dst data]
   (reduce
    (fn [dst item]
      (update dst (classifier item) (fn [dst] (conj (or dst empty-dst) item))))
    {}
    data)))

(defn int-floor [x]
  (int (Math/floor x)))

(defn int-ceil [x]
  (int (Math/ceil x)))

(defn greater [x]
  (inc (int-floor x)))

(defn greater-or-equal [x]
  (let [y (int-floor x)]
    (if (< y x)
      (inc y)
      y)))

(defn less [x]
  (dec (int-ceil x)))

(defn less-or-equal [x]
  (let [y (int-ceil x)]
    (if (< x y)
      (dec y)
      y)))

(defn rotate
  "Rotate a collection"
  [src steps]
  (let [src (vec src)
        n (count src)]
    (mapv (fn [index] (nth src (mod (+ steps index) n)))
          (range n))))

(defn check-arg-map [arg-spec arg-map]
  (check map? arg-spec)
  (check map? arg-map)
  (doseq [[k checker] arg-spec]
    (if (contains? arg-map k)
      (let [v (get arg-map k)]
        (cond
          (keyword? checker) (validate checker v)
          (not (checker v)) (throw (ex-info "Failed arg-spec"
                                            {:key k
                                             :value v}))))
      (throw (ex-info "Missing key"
                      {:key k}))))
  ::success)

(defmacro check-arg-list [predicates args-expr]
  (check sequential? predicates)
  (let [n (count predicates)
        syms (take n (repeatedly gensym))]
    `(let [args# ~args-expr]
       (if (= ~n (count args#))
         (let [[~@syms] args#]
           ~@(map (fn [i pred sym]
                    `(if (not (~pred ~sym))
                       (throw (ex-info "Bad argument"
                                       {:index ~i
                                        :predicate (quote ~pred)
                                        :value ~sym}))))
                  (range) predicates syms)
           args#)
         (throw (ex-info "Wrong length of arguments"
                         {:expected (quote ~predicates)
                          :provided args#}))))))

(defn clamp [x lower upper]
  (min upper (max x lower)))


(defn maximize [ordered-items0 valid-fn]
  (let [ordered-items (vec ordered-items0)
        n (count ordered-items)]
    (if (< 0 n)
      (let [mid (int (/ n 2))
            item (nth ordered-items mid)]
        (if (valid-fn item)
          (if (= 1 n)
            item
            (maximize (subvec ordered-items mid n) valid-fn))
          (maximize (subvec ordered-items 0 mid) valid-fn))))))

(defn assign-indices
  ([values]
   (assign-indices values :index))
  ([values index-key]
   (map (fn [dst i] (assoc dst index-key i))
        values
        (range))))

(defn interpolate [x [x0 x1] [y0 y1]]
  (let [lambda (/ (- x x0) (- x1 x0))]
    (+ (* (- 1 lambda) y0)
       (* lambda y1))))

(defn repeat-cat [s]
  (let [v (vec s)
        n (count s)]
    (map #(nth v (mod % n)) (range))))

(defn butlast-vec [v]
  (let [v (vec v)]
    (subvec v 0 (dec (count v)))))


(defn- solve-key-indirection [m k]
  (let [v (get m k)]
    (if (contains? m v)
      (let [m (solve-key-indirection m v)]
        (assoc m k (get m v)))
      m)))

(defn solve-key-indirections [m]
  (reduce
   solve-key-indirection
   m
   (keys m)))

(defn map-vals [f m]
  (into (empty m)
        (for [[k v] m]
          [k (f v)])))

(defn check-get [m k]
  (if (contains? m k)
    (get m k)
    (throw (ex-info "Missing key"
                    {:map m
                     :key k}))))

(defmacro dout [& args]
  `(do
     ~@(for [arg args]
         `(let [arg# ~arg]
            (println ~(str arg) "=" arg#)
            arg#))))

(defn nested-map [f data]
  (for [x data]
    (if (coll? x)
      (nested-map f x)
      (f x))))

(defn copy-keys [dst ks src]
  (merge dst (select-keys src ks)))

(defn replace-first [dst-collection pref]
  (reduce into (empty dst-collection)
          [pref
           (drop (count pref) dst-collection)]))
