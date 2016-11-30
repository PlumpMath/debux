(ns debux.lab
  (:require (clojure [string :as str]
                     [zip :as zip]
                     [pprint :as pp] )))

(use 'debux.core)

(defn insert-d [loc]
  (cond
    (zip/end? loc) (zip/root loc)
        
    (seq? (zip/node loc))
    (recur (-> (zip/replace loc (concat ['d] [(zip/node loc)]))
               zip/down zip/right zip/down zip/right))

    :else
     (recur (zip/next loc))))

(defn remove-d [loc]
  (let [node (zip/node loc)]
    (cond
      (zip/end? loc) (zip/root loc)
        
      (and (seq? node)
           (= 'd (first node)))
      (recur (zip/replace loc (second node)))
      
      :else
      (recur (zip/next loc)) )))

(defmacro d [form]
  `(let [return# ~form
         print# '~(remove-d (zip/seq-zip form))]
     (println print# "=>" return#)
     return#))

(defmacro dbgn [form]
  (insert-d (zip/seq-zip form)))


(comment

(def z (zip/seq-zip '(* c (+ a b))))
(insert-d z)
; => (d (* c (d (+ a b))))

(def z2 (zip/seq-zip (insert-d z)))
(remove-d z2)
; => (* c (+ a b))

(d (* c (+ a b)))
; >> (* c (+ a b)) => 25
; => 25

(dbgn (* c (+ a b)))
; >> (+ a b) => 5
; >> (* c (+ a b)) => 25
; => 25

(dbgn (let [grade 85]
        (cond
          (>= grade 90) "A"
          (>= grade 80) "B"
          (>= grade 70) "C"
          (>= grade 60) "D"
          :else "F")))
; >> (>= grade 90) => false
;    (>= grade 80) => true
;    (cond (>= grade 90) A (>= grade 80) B (>= grade 70) C (>= grade 60) D :else F) => B
;    (let [grade 85] (cond (>= grade 90) A (>= grade 80) B (>= grade 70) C (>= grade 60) D :else F)) => B
; => B

(dbgn (defn pos-neg-or-zero
  "Determines whether or not n is positive, negative, or zero"
  [n]
  (cond
    (< n 0) "negative"
    (> n 0) "positive"
    :else "zero")))

(pos-neg-or-zero 10)
; >> (< n 0) => false
;    (> n 0) => true
;    (cond (< n 0) negative (> n 0) positive :else zero) => positive
; => "positive"

) ; end of comment
