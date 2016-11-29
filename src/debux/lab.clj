(ns debux.lab
  (:require (clojure [string :as str]
                     [walk :as walk]
                     [pprint :as pp] )))

;(use 'debux.core)

;; ;;; For internal debugging

(defmacro ^:private dbg_
  "The internal macro to debug dbg macro.
   <form any> a form to be evaluated"
  [form]
  `(let [return# ~form]
     (println ">> dbg_:" (pr-str '~form) "=>" return# "<<")
     return#))

(defmacro d0
  [x]
  ;(dbg_ (type x))
  `(let [x# ~x]
     (println (:form (dbg_ (meta '~x))) "=>" x#)
     x#))

(defmacro d
  [x]
  `(let [x# ~(vary-meta x assoc :form '~x)]
     (println (:form (meta (dbg_ x#))) "=>" x#)
     x#))

(defmacro dbgn [form]
  `(d ~(vary-meta form
                  assoc :form `~form)))


;;; For test

(def a 2)
(def b 3)
(def c 5)

(d (* c (d (+ a b))))

(dbgn (* c (dbgn (+ a b))))
; >> (* c (+ a b)) => 25

(dbgn a)
; >> a => 2


(dbgn (let [a 10 b 20] (dbgn (+ a b))))
; >> (let [a 10 b 20] (+ a b)) => 30

(dbgn a)
; >> a => 2
