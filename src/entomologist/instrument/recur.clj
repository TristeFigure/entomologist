(ns entomologist.instrument.recur
  (:require [rewrite-clj.node :as n]
            [entomologist.rewrite-clj.complements :refer [token-node?]]))

(deftype RecurArguments [args])

(defmacro fake-recur [& args]
  `(RecurArguments. [~@args]))

;; this is called at runtime/readtime
;; and we need to know the number of args of the recur form at instrumentation 
;; time since the compiler/reader needs to know this information to do its job
;; Or in other words : we can't use apply at runtime (recur is a special form)
;; nor use eval or any other kind of shenanigans because of compiler-related
;; wizardry that need, well, to happen at compile time
(defmacro handle-faked-recur [args-count expr]
  (let [ret-sym (gensym "ret-")
        recur-form (cons 'recur
                         (for [n (range args-count)]
                           `(get (.args ~ret-sym) ~n)))]
    `(let [~ret-sym ~expr]
       (if (instance? RecurArguments ~ret-sym)
         ~recur-form
         ~ret-sym))))


