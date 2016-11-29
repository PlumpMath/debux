(ns debux.lab
  (:require (clojure [string :as str]
                     [zip :as zip]
                     [pprint :as pp] )))

(use 'debux.core)

;;; For internal debugging

(defmacro ^:private dbg_
  "The internal macro to debug dbg macro.
   <form any> a form to be evaluated"
  [form]
  `(let [return# ~form]
     (println ">> dbg_:" (pr-str '~form) "=>" return# "<<")
     return#))

(defmacro d [x f]
  `(let [x# ~x]
     (println ~f "=>" x#)
     x#))

(def a 2)
(def b 3)
(def c 5)

;; input (dbgn (* c (+ a b)))
;; output
(d (* c (d (+ a b)
           '(+ a b)))
   '(* c (+ a b)))

(defmacro dbgn [form])

(def z (zip/seq-zip '(* c (+ a b))))

(defn insert-d [loc]
  (cond
    (zip/end? loc) (zip/root loc)
        
    (seq? (zip/node loc))
    (recur (-> (zip/replace loc (concat ['d] [(zip/node loc)]))
               zip/down zip/right zip/down zip/right))

    :else
     (recur (zip/next loc))))

(insert-d z)
; => (d (* c (d (+ a b))))


(def z2 (zip/seq-zip (insert-d z)))

(defn remove-d [loc]
  (let [node (zip/node loc)]
    (cond
      (zip/end? loc) (zip/root loc)
        
      (and (seq? node)
           (= 'd (first node)))
      (recur (zip/replace loc (second node)))
      
      :else
      (recur (zip/next loc)) )))

(remove-d z2)
; => (* c (+ a b))



