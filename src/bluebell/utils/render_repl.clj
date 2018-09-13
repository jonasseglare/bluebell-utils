(ns bluebell.utils.render-repl
  "A small DSL for rendering REPL interactions suitable for publishing"
  (:require [clojure.spec.alpha :as spec]
            [clojure.pprint :as pp]))

(def disp? (partial = :disp))

(spec/def ::item (spec/alt
                  :comment (spec/cat :prefix #{:comment :cmt}
                                     :value string?)
                  :break #{:break}
                  :instruction (spec/cat :disp? (spec/? disp?)
                                         :value (complement disp?))))

(spec/def ::body (spec/* ::item))

(defn render-instruction [[type instr]]
  (let [{:keys [disp? value]} instr
        rsym (gensym)]
    (case type
      :break `(println "")
      :comment `(println (str  ";; " ~(:value instr)))
      `(let [~rsym ~value]
         (print
          ~(binding [pp/*print-pprint-dispatch*
                     pp/code-dispatch]
             (with-out-str
               (pp/pprint value))))
         ~(if disp?
            `(println (str ";;=> " ~rsym)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro render [& body0]
  (let [body (spec/conform ::body body0)]
    (assert (not= ::spec/invalid body))
    `(do
       (println "\n")
       ~@(map render-instruction body))))


(defn render-example []
  (render 
   (def a 9)
   :break

   :disp :namespaced/keyword

   :break
   
   :comment "Add them together"
   :disp (+ a a)))
