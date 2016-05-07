(ns debux.lab
  (:require (clojure [string :as str]
                     [pprint :as pp] )))

;;; For internal debugging

(defmacro ^:private dbg_
  "The internal macro to debug dbg macro.
   <form any> a form to be evaluated"
  [form]
  `(let [return# ~form]
     (println ">> dbg_:" (pr-str '~form) "=>" return# "<<")
     return#))

(defmacro d
  [form]
  `(let [form# ~form]
     (println (:form (meta '~form)) "=>" form#)
     form#))

(let [a 2
      b 3
      c 5]
  (d ^{:form (* c (+ a b))}
     (* c (d ^{:form (+ a b)} (+ a b)) )))

