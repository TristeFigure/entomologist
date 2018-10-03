(ns entomologist.instrument.wrap
  (:require [entomologist.rewrite-clj.complements
             :refer [template]]
            [entomologist.instrument.context
             :refer [context-sym install-context scope-context
                     insect-name-sym with-insect-name
                     ns-sym with-ns time-mark synth-execution-context]
             :reload true]
            [entomologist.instrument.static :refer [instrument-static]]
            [entomologist.instrument.symbol-info :refer [symbol-info]]
            [entomologist.instrument.runtime :refer [instrument-runtime]]
            [entomologist.rewrite-clj.complements
             :refer [token-node?]]))

;; From top to bottom.

(def ^:dynamic *instrument-fn-sym-in-out?* true)

(declare wrap-instrument
         wrap-instrument-symbol
         wrap-instrument-expr
         wrap-instrument-symbol
         wrap-instrument-fn)

(defn wrap-instrument
  ([n]
   (wrap-instrument n n))
  ([n original-n]
   (if-let [sym (and (token-node? original-n)
                     (symbol? (:value original-n))
                     (:value original-n))]
     (wrap-instrument-symbol sym n original-n)
     (wrap-instrument-expr n original-n))))

(defn wrap-instrument-expr [n original-n]
  (template
    `(let [~'YYY ~'XXX]
       (instrument-runtime ~ns-sym
                           ~insect-name-sym
                           ~(:path original-n)
                           (synth-execution-context ~context-sym)
                           ~'YYY)
       ~'YYY)
    {:XXX (instrument-static n)
     :YYY (gensym "instr-")}))

(defn wrap-instrument-symbol* [sym ns insect-name cst-pth ctx v]
  "Without emulating the whole evolution of the environment at compile time
  we can only know for sure a symbol is documented, i.e. globally defined,
  and not shadowed by some local var only by querying for it at runtime.
  wrap-instrument-symbol* is meant to be run at runtime. It is synthesized
  by wrap-instrument-symbol (without the '*').
  If the symbol is local, its value rather than its documentation is processed."
  (if-let [si (symbol-info sym)]
    (instrument-runtime ns insect-name cst-pth [:static :doc] si)
    (instrument-runtime ns insect-name cst-pth
                        (synth-execution-context ctx)
                        v)))

(defn wrap-instrument-symbol [sym n original-n]
  (template
    (if *instrument-fn-sym-in-out?*
      `(let [instr# ~'XXX]
         (wrap-instrument-symbol* '~sym ~ns-sym ~insect-name-sym
                                  ~(:path original-n) ~context-sym instr#)
         (if (fn? instr#)
           (wrap-instrument-fn instr# ~ns-sym ~insect-name-sym
                               ~(:path original-n) ~context-sym)
           instr#))
      `(let [instr# ~'XXX]
         (wrap-instrument-symbol* '~sym ~ns-sym ~insect-name-sym
                                  ~(:path original-n) ~context-sym instr#)
         instr#))
    {:XXX (instrument-static n)}))

(defmacro wrap-instrument-fn [f ns insect-name cst-pth ctx]
  `(fn [& args#]
     (let [ret# (apply ~f args#)]
       ;; TODO: why do we add this scope context ?
       (scope-context
         {"& args" {:value args# :path ~cst-pth}}
         (instrument-runtime ~ns ~insect-name ~cst-pth
                             (synth-execution-context ~ctx)
                             ret#))
       ret#)))











(defn wrap-limit-instrumention [n]
  (template `(do (println "CONTEXT - SYM :" ~context-sym)
                 ~'XXX)
            {:XXX n})
  #_(let [path (:path n)
        ]))

