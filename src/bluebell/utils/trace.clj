(ns bluebell.utils.trace
  (:require [clojure.spec.alpha :as spec]
            [bluebell.utils.core :as utils]
            [bluebell.utils.specutils :as sutils]))

(defn trace-fn []
  (let [state (atom [])]
    (fn
      ([]
       (deref state))
      ([value]
       (swap! state (fn [state] (conj state [value (System/nanoTime)])))))))

(defn record [dst data]
  (when dst
    (dst data))
  data)

(defn begin [dst tag]
  (record dst [:begin tag]))

(defn end [dst tag]
  (record dst [:end tag]))

;[[:a 11639427761547] [[:begin :b] 11639427772743] [[:begin :c] 11639427778198] [:kattskit 11639427782285] [[:end :c] 11639427788288] [[:end :b] 11639427791153] [:d 11639427795402]]

;;;;;


(defn time-pair [value-spec]
  (spec/spec (spec/cat :value value-spec
                       :time ::nanoseconds)))

(spec/def ::nanoseconds number?)

(defn pair [tag-spec value-spec]
  (spec/spec (spec/cat :tag tag-spec
                       :value value-spec)))

(defn kw-vec [kw]
  (pair #{kw} any?))

(spec/def ::begin (time-pair (kw-vec :begin)))
(spec/def ::end (time-pair (kw-vec :end)))
(spec/def ::block (spec/cat :begin (spec/spec ::begin)
                            :trace ::trace
                            :end (spec/spec ::end)))

(spec/def ::begin-or-end (spec/or :begin (kw-vec :begin)
                                  :end (kw-vec :end)))

(defn not-begin-or-end [x]
  (not (spec/valid? ::begin-or-end x)))



(spec/def ::single-item (spec/and
                         (time-pair not-begin-or-end)))


(spec/def ::trace-part (spec/alt :block ::block
                                 :item ::single-item))

(spec/def ::trace (spec/* ::trace-part))

(defn consistent-block? [block]
  (= (-> block
         :begin
         :value
         :value)
     (-> block
         :end
         :value
         :value)))

(defn list-invalid-blocks
  ([trace-data] (list-invalid-blocks [] trace-data))
  ([dst trace-data]
   (transduce
    (comp (map (fn [[item-type item-data]]
                 (if (= item-type :block)
                   item-data)))
          (filter identity)
          (map (fn [block]
                 (list-invalid-blocks
                  (if (consistent-block? block)
                    []
                    [block])
                  (:trace block))))
          cat)
    conj
    dst
    trace-data)))

(defn parse-spec [trace-data]
  (sutils/force-conform ::trace trace-data))

(defn format-block-bound [bound]
  {:time (:time bound)
   :value (-> bound :value :value)})

(defn format-invalid-block [block]
  {:begin (format-block-bound (:begin block))
   :end (format-block-bound (:end block))})

(defn check-no-invalid-blocks [parse-spec]
  (let [invalid (list-invalid-blocks parse-spec)]
    (if (not (empty? invalid))
      (throw (ex-info "Invalid blocks"
                      {:blocks (map format-invalid-block invalid)
                       :parse-spec parse-spec})))
    parse-spec))

(defn parse [trace-data]
  (-> trace-data
      parse-spec
      check-no-invalid-blocks))

(declare disp-parsed)

(defn ns2str [ns]
  (format "%.3g ms" (* 1.0e-6 (double ns))))

(defn disp-item [item indent cfg]
  (println (str indent (:value item)
                " at " (ns2str (- (:time item)
                                  (:start cfg)))
                (if (contains? item :dur)
                  (str " (dur=" (ns2str (:dur item)) ")")
                  ""))))

(defn disp-block-bd [bd indent cfg]
  (disp-item (update bd :value :value) indent cfg))

(defn disp-block [block indent cfg]
  (let [dur (- (-> block :end :time)
               (-> block :begin :time))]
    (disp-block-bd (:begin block) indent cfg)
    (disp-parsed (:trace block) (str indent " | ") cfg)
    (disp-block-bd (merge (:end block)
                          {:dur dur}) indent cfg)))

(defn disp-parsed [trace-data indent cfg]
  (doseq [[item-type item-data] trace-data]
    (case item-type
      :block (disp-block item-data indent cfg)
      :item (disp-item item-data indent cfg))))

(defn start-time [trace]
  (-> trace
      first
      second))


(defn disp-trace
  ([trace] (disp-trace trace {}))
  ([trace cfg0]
   (if (fn? trace)
     (disp-trace (trace))
     (-> trace
         parse
         (disp-parsed "" (merge cfg0 {:start (start-time trace)}))))))
