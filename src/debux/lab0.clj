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

(defmacro d
  [x]
  ;(dbg_ (type x))
  `(let [x# ~x]
     (println (:form (meta '~x)) "=>" x#)
     x#))

(def a 2)
(def b 3)
(def c 5)

;; input (dbgn (* c (+ a b)))
;; output
(d ^{:form (* c (+ a b))}
     (* (d ^{:form c} c)
        (d ^{:form (+ a b)}
           (+ (d ^{:form a} a)
              (d ^{:form b} b) ))))

(defmacro dd0 []
  (d ~(with-meta (* (d ~(with-meta c {:form c}))
                     (d ~(with-meta (+ (d ~(with-meta a {:form a}))
                                       (d ~(with-meta b {:form b})))
                           {:form (+ a b)}))
                     {:form (* c (+ a b))}))))

(defmacro dd []
  `(d (with-meta (* (d (with-meta c {:form c}))
                    (d (with-meta (+ (d (with-meta a {:form a}))
                                     (d (with-meta b {:form b})))
                         {:form (+ a b)}))
                    {:form (* c (+ a b))}))))
;(dd)

(defn dispatch
  [node]
  ;(dbg_ node)
  (cond
    (dbg_ (list? node))
    (eval `(dbg_ ~node))

    :else (dbg_ node)))

;; (defmacro dispatch
;;   [node]
;;   ;(dbg_ node)
;;   (cond
;;     (list? node)
;;     `(do (dbg ~node) ~node)

;;     :else `~node))

;; (dispatch1 (* a b))
;; (dispatch a)
;; (dispatch 10)


(defn tree-walk
  [tree]
  (walk/prewalk #'dispatch tree))

;(tree-walk '(* c (+ a b)))

(defmacro dbgn [form]
  `(d ~(with-meta form {:form form})))

;; (dbgn (* c (+ a b)))
;; (dbgn a)
