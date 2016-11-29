(ns debux.lab
  (:require (clojure [string :as str]
                     [walk :as walk]
                     [pprint :as pp] )))

;; For internal debugging
(defmacro ^:private dbg_
  "The internal macro to debug dbg macro.
   <form any> a form to be evaluated"
  [form]
  `(let [return# ~form]
     (println ">> dbg_:" (pr-str '~form) "=>" return# "<<")
     return#))

(def a 2)
(def b 3)
(def c 5)

(defn- dispatch
  [node]
  (cond
    (list? node)
    (do (eval `(dbg_ ~node))
        node)

    (and (symbol? node)
         (not (fn? (eval `~node))))
    (do (eval `(dbg_ ~node))
        node)

    :else node))

(defn- tree-walk
  [tree]
  (walk/postwalk dispatch tree))


;; dbg for nested expressions
(defmacro dbgn [form]
  (tree-walk form))

(dbgn (* c (+ a b)))
; >> dbg_: c => 5 <<
; >> dbg_: a => 2 <<
; >> dbg_: b => 3 <<
; >> dbg_: (+ a b) => 5 <<
; >> dbg_: (* c (+ a b)) => 25 <<


(let [a 10 b 20 c 30]
  (dbgn (* c (+ a b))))
;   2. Unhandled clojure.lang.Compiler$CompilerException
;      Error compiling work/philos/debux/src/debux/lab.clj at (51:3)
;   
;   1. Caused by java.lang.UnsupportedOperationException
;      Can't eval locals
;   
;                Compiler.java: 5943  clojure.lang.Compiler$LocalBindingExpr/eval
;                Compiler.java: 6932  clojure.lang.Compiler/eval
;                Compiler.java: 6890  clojure.lang.Compiler/eval
;                     core.clj: 3105  clojure.core/eval
;                     core.clj: 3101  clojure.core/eval
;                ......

(let [a 10 b 20 c 30]
  (eval `(* ~c (+ ~a ~b))))
; => 900

(let [a 10 b 20 c 30]
  (eval `(dbgn (* ~c (+ ~a ~b)))))
; => 900
