(ns epicea.tag.core)

(defn tagged?
  ([tag] 
   (fn [x]
     (tagged? tag x)))
  ([tag x]
   (if (vector? x)
     (let [[a b] x]
       (= a tag)))))

(defn pair? [x]
  (and (vector? x)
       (= 2 (count x))))

(defn value [x]
  (second x))

(defn tag 
  ([t x] [t x])
  ([t] (fn [x] (tag t x))))

(defn get-tag [x]
  (first x))

(defn tag-like [proto x]
  (tag (get-tag proto) x))


;;; Convenience definitions  
(def tag-success (tag :success))
(def tag-failure (tag :failure))
(def success? (tagged? :success))
(def failure? (tagged? :failure))
