(ns entomologist.instrument.utils
  (:require [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [entomologist.instrument.context :as context]
            [entomologist.rewrite-clj.complements
             :refer [template token-node? vector-node? not-code-node? map-node?
                     keyword-node? remove-rights]]
            [entomologist.config :refer [config]]
            [entomologist.instrument.wrap :refer [wrap-instrument]
             :reload true]))

;; Against cyclical depencies
(def ^:private instrument
  (ns-resolve 'entomologist.instrument.core
              'instrument))

(defn maybe-doall [x]
  (if (and (:doall @config) (seq? x))
    (doall x)
    x))

(defn wrap-in-maybe-doall [x]
  (template `(maybe-doall ~'XXX)
            {:XXX x}))

(defn destructuring-exprs [n]
  (cond
    (token-node? n)   [n]
    ;; for vectors, we keep the keyword of the bindings in the displayed result
    ;; e.g. :as
    (vector-node? n)  (let [wo-ws (remove not-code-node?
                                          (:children n))]
                        (conj (mapcat destructuring-exprs wo-ws)
                              (assoc n :children wo-ws)))
    ;; for maps, we get rid of any :keys, :or and :as keys and display the map
    ;; value (if :as is used), or a map of bindings syms to bound values
    (map-node? n)     (let [wo-ws (remove not-code-node?
                                          (:children n))
                            m (->> wo-ws
                                   (partition 2)
                                   (map (fn [[k v]]
                                          [(if (keyword-node? k)
                                             (:k k)
                                             k)
                                           v]))
                                   (into {}))
                            ;; either the value of the map (if :as is present)
                            ;; or the map of bindings syms to bound values 
                            mm (assoc
                                 (if-let [x (:as m)]
                                   x
                                   (template
                                     '(apply array-map CHILDREN)
                                     {:CHILDREN
                                      (n/vector-node
                                        (concat
                                          ;; get bind syms from :keys
                                          (mapcat
                                            (fn [sym-node]
                                              (let [s (:value sym-node)
                                                    k (keyword s)]
                                                [(n/quote-node s) s]))
                                            (->> m :keys :children
                                                 (remove not-code-node?)))
                                          ;; and also from the map itself
                                          (mapcat
                                            (fn [[sym-node map-key-node]]
                                              [(n/quote-node sym-node)
                                               sym-node])
                                            (-> m (dissoc :keys :as :or)))))}))
                                 :path (:path n))]
                        (apply concat
                               (destructuring-exprs (:keys m))
                               (destructuring-exprs (:as m))
                               [mm]
                               (->> (dissoc m :keys :as :or)
                                            (map (comp destructuring-exprs
                                                       key)))))))

(defn instrument-form-with-args-and-body
  "Instrument one args/body pair.
  - args-loc : a zipper pointing to an args vector
  - before-body-loc : if none is provided, defaults to args-loc
  - multibodied? : whether this form is one body of a multibodied form
  
  TODO : keep ?
  - instrument-top-level? : whether to instrument the containing form"
  ([args-loc multibodied?]
   (instrument-form-with-args-and-body args-loc args-loc multibodied?))
  ([args-loc before-body-loc multibodied?]
   (let [;; we empty the body exprs and keep them aside
         [before-body-loc body-nodes] (remove-rights before-body-loc)
         ;; and make sure any instrumented node gets the fn scope context
         destr-exprs (destructuring-exprs (z/node args-loc))
         scope-paths (context/scope-paths destr-exprs)
         ret-sym (gensym "ret-")
         before-body-loc
         (-> before-body-loc
             (z/insert-right
               (template `(context/scope-context
                            ~'AAA
                            ;; we'll put our code inside the do form
                            (let [~ret-sym (do)]
                              ;; and we'll instrument the result here
                              ~'BBB
                              ;; and maybe we'll instrument the (fn ...) form of
                              ;; multibodied fns
                              ~'CCC
                              ~ret-sym))
                         {:AAA scope-paths
                          :BBB (wrap-instrument (n/token-node ret-sym)
                                                (-> args-loc
                                                    z/up z/node))
                          :CCC (when multibodied?
                                 (wrap-instrument
                                   (n/token-node ret-sym)
                                   (-> args-loc
                                       z/up z/up z/node)))}))
             (z/insert-right (n/newline-node "\n")))
         ;; then we insert what's necessary to instrument the args vector exprs
         do-loc (z/find-value before-body-loc z/next 'do)
         loc (if (empty? body-nodes) 
               ;; empty fns return nil. But since we instrumented the fn args by
               ;; adding stuff in the body, we need to add a terminal nil to the
               ;; body if it was originally empty.
               (-> do-loc (z/insert-right (n/token-node nil)))
               (loop [xloc do-loc
                      [s & ss] destr-exprs]
                 (if s
                   (recur (-> xloc
                              (z/insert-right (wrap-instrument s))
                              (z/insert-right (n/newline-node "\n"))
                              z/right)
                          ss)
                   xloc)))
         ;; finally we add the original body forms back, instrumenting them
         ;; beforehand
         loc (loop [xloc loc
                    [b & bs] body-nodes]
               (if b
                 (recur
                   (-> xloc
                       (z/insert-right (instrument b))
                       (z/insert-right (n/newline-node "\n"))
                       z/right)
                   bs)
                 xloc))
         args-loc-pth (clojure.zip/path args-loc)]
     ;; and back to args-loc
     (z/skip z/prev #(not= (clojure.zip/path %) args-loc-pth)
             loc))))

