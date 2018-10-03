(ns entomologist.instrument.context
  (:require [entomologist.rewrite-clj.complements :refer [template]]))

;; I initially used dynamic vars and the binding macro,
;; but this caused problems of course (thread local, ehh). Instead, we'll use
;; lexical bindings along with normal vars containing those lexical symbols

;; TODO: explicitly deftype contexts. Measure performance difference.
(def insect-name-sym
  (gensym "insect-name-"))

(defmacro with-insect-name [name & body]
  `(let [~insect-name-sym ~name]
     ~@body))

(def ns-sym
  (gensym "ns-"))

(defmacro with-ns [ns & body]
  `(let [~ns-sym ~ns]
     ~@body))
 
(def context-sym
  (gensym "context-"))

(defmacro install-context [& body]
  `(let [~context-sym {}]
     ~@body))

(defmacro update-in-context [ks f args & body]
  `(let [~context-sym (update-in ~context-sym ~(vec ks) ~f ~@args)]
     ~@body))

(defmacro scope-context
  "Context used to store particular values a cst node can take at a specific
  time during execution. It happens when a form's evaluation brings new symbols 
  in the env, as fn, loop, for etc ... do."
  [scope-paths & body]
  `(update-in-context
     [:scope] (fnil concat []) [[~scope-paths]]
     ~@body))

(defmacro limit-instrumentation [ns insect-name path context name]
  )

(defn scope-paths [destruct-exprs]
  (mapv :path destruct-exprs))

;; TODO: only used by datascript-store. Remove ?
(defn subcontext? [ctxa ctxb]
  (= (first ctxa)
     (first ctxb)))

;; TODO: and what about an id as well ? Can two threads get the same nanoTime ?
(defn time-mark []
  [(new java.util.Date) (System/nanoTime)])

(defn synth-execution-context [ctx]
  (concat [:runtime :exec] (or (:scope ctx) [[]]) [(time-mark)]))


