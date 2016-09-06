(ns debux.lab
  (:require (clojure [string :as str]
                     [walk :as walk]
                     [pprint :as pp] )))

;(use 'debux.core)

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

(defn dispatch
  [node]
  ;(dbg_ node)
  (cond
    (list? node)
    (do (eval `(dbg_ ~node))
        node)

    (and (symbol? node) (not (fn? (eval node))))
    (do (eval `(dbg_ ~node))
        node)

    :else node))

(defn tree-walk
  [tree]
  (walk/postwalk dispatch tree))

;(tree-walk '(* c (+ a b)))

(defmacro dbgn [form]
  (tree-walk form))

;; (dbgn (* c (+ a b)))
;; (dbgn (let [a 10 b 20 c 30] (+ a b c)))
