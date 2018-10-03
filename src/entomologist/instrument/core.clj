(ns entomologist.instrument.core
  (:use clojure.pprint)
  (:require [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [rewrite-clj.zip.base :refer [edn]]
            [entomologist.rewrite-clj.complements
             :refer [template seq-node? not-code-node? def-form-node?
                     list-node? string-node? token-node? vector-node?
                     map-node? keyword-node? quote-node? map-code
                     remove-rights]]
            [entomologist.cst :as cst]
            [entomologist.instrument.runtime :refer [instrument-runtime]
             :reload true]
            [entomologist.store :as store
             :reload true]
            [entomologist.instrument.symbol-info
             :refer [symbol-info]]
            [entomologist.instrument.utils :as utils
             :reload true]
            [entomologist.instrument.wrap
             :refer [wrap-instrument *instrument-fn-sym-in-out?*
                     wrap-instrument-expr]
             :reload true]
            [entomologist.instrument.context :as context
             :reload true]
            [entomologist.instrument.recur
             :refer [fake-recur handle-faked-recur]
             :reload true]))

;; TODO: maybe too much dependence on entomologist.instrument.wrap and
;; entomologist.instrument.context

;; TODO: actually test whether you can shadow, say the inc var, and have
;; it instrumented not as clojure.core/inc. with-redefs etc ...

;; TODO: refactor the way scope-context (and potentially other contexts) are
;; produced. Their are forms that modify the scope and others that do not.

;; TODO: what happens, when, say, a macro is changing the env, but is not listed in
;; in the list of forms subject to a call to scope-contex ?


;; For the code follows this very logic, it is presented from top to bottom.

(declare instrument-node
         instrument-coll-node
         instrument-linear-collection
         instrument-associative
         instrument-form
         instrument-anonymous-form)

;  _  ,_ |-               . ,_ |-
; (/_ || |_ |` \/   |) () | || |_
;              /    |
(defn instrument [n]
  (if (seq-node? n)
    (instrument-coll-node n)
    (instrument-node n)))

; |  _     [`   ,_     |  _   _
; | (/_ (| |    || () (| (/_ _\
;
(defn instrument-node [n]
  (wrap-instrument n))

;  _    | |  _   _ |- .    ,_  _
; (_ () | | (/_ (_ |_ | () || _\
;
(defn instrument-coll-node [n]
  (case (:tag n)
    :list (instrument-form n)
    :vector (instrument-linear-collection n)
    :set (instrument-linear-collection n)
    :map (instrument-associative n)))

(def ^:dynamic *instrument-linear-coll-node?*
  true)

(defn instrument-linear-collection [n]
  (->> (:children n)
       (map-code instrument)
       (assoc n :children)
       ((fn [x]
          (if *instrument-linear-coll-node?*
            (wrap-instrument x n)
            x)))))
;; TODO
(defn instrument-associative [n]
  (wrap-instrument
    (update-in n [:children]
               (partial map-code instrument))))

; [`       ,_   _
; |  () |` ||| _\
;
(defn rewrite-form [n first-rewriter rest-rewriter]
  (assert (list-node? n))
  (let [loc (-> n edn   ;; (abc xyz...)
                z/down  ;; abc
                (z/edit* first-rewriter))]
    (loop [l loc]
      (if-let [new-l (z/right l)]
        (recur (z/edit* new-l rest-rewriter))
        (-> l z/up z/node)))))

(defn form-dispatch [n]
  (let [x (->> n :children
               (remove not-code-node?)
               first)
        v (:value x)
        has-method? (contains? (methods instrument-form) v)]
    (cond (seq-node? x)
          :first-is-seq
          
          (and (some-> v resolve meta :macro)
               (not has-method?))
          :first-is-macro
          
          (and (special-symbol? v)
               (not has-method?))
          :first-is-special
          
          :else v)))

(defmulti instrument-form form-dispatch)

(defn instrument-form-default [n]
  (let [instr (rewrite-form n
                            (fn [n]
                              (if (token-node? n)
                                (binding [*instrument-fn-sym-in-out?* false]
                                  (instrument n))
                                (instrument n)))
                            instrument)]
    (wrap-instrument-expr instr n)))

;; TODO ; finish with instrument-form-default
(defmethod instrument-form :default [n]
  (instrument-form-default n))

;; forms whose first element is also a form
(defmethod instrument-form :first-is-seq [n]
  (instrument-form-default n))

(defmethod instrument-form :first-is-macro [n]
  (let [x (-> n edn z/down z/node)]
    (wrap-instrument-expr
      (template `(do (when (resolve '~(:value x))
                       (instrument-runtime ~context/ns-sym
                                           ~context/insect-name-sym
                                           ~(:path x)
                                           ~context/context-sym
                                           (symbol-info '~(:value x))))
                     ~'XXX)
                {:XXX (rewrite-form n identity instrument)})
      n)))

; ~|~ |_  _    ,_        ,_ . [`    . ,_         |     _  _
;  |  || (/_   ||| (| (| || | |  \/ | || (|   (| | (| _\ _\
;                     _|         /       _|   _|
;; the magifying glass form itself gets rewritten. How ? Everything is
;; detailed in this section

(defmethod instrument-form '-o [n]
  (rewrite-form
    n
    (constantly (n/token-node 'do))
    (fn [x]
      (template
        `(context/with-ns (find-ns '~(symbol (str *ns*)))
           (context/install-context
             ~'XXX))
        {:XXX
         (if (def-form-node? x)
           (instrument-form x);; propagates to instrument-form 'def/'defn/etc...
                              ;; we need to go by this supplementary indirection
                              ;; layer because we also want to mark as insects
                              ;; def forms that are not 'right under' the
                              ;; magnifying glass, but deeper in the code,
                              ;; for instance beneath a let form.
           (instrument-anonymous-form x))}))))

;    ,_    ,_    ,_         _   [`       ,_   _
; (| || () || \/ ||| () L| _\   |  () |` ||| _\
;             /

(defn instrument-anonymous-form [n]
  (let [with-paths (cst/add-paths n)
        name (store/remark-insect! *ns* with-paths)
        instr-sym (gensym "instr-")]
    (template
      `(context/with-insect-name
         ~name
         ~'XXX)
      {:XXX (-> with-paths
                instrument
                utils/wrap-in-maybe-doall)})))

;  |  _  [`
; (| (/_ |
;

(defmethod instrument-form 'def [n]
  (let [with-paths (cst/add-paths n)
        name (store/remark-insect! *ns* with-paths)
        zip (edn with-paths) ;;(def name ...)
        def-sym-node (-> zip z/down z/node)
        def-sym (:value def-sym-node)
        x-loc (-> zip        
                  z/down     ;; def
                  z/right    ;; name
                  z/right)]  ;; docstring or value
    (-> (if (-> x-loc z/node string-node?)
          (z/right x-loc)
          x-loc)
        (z/edit* (comp utils/wrap-in-maybe-doall instrument)) ;; the value
        z/up ;; (def name ...)
        z/node
        (#(template
            `(context/with-insect-name
               ~name
               ~'AAA
               ~'BBB)
            {:AAA (wrap-instrument (n/quote-node def-sym) def-sym-node)
             :BBB %})))))

;  |  _  [` ,_
; (| (/_ |  ||
;
;; TODO : handle-faked-recur : what happens with var-args ?
;; TODO : adding & [... :as args#] will change doc info
;;        (debugger not transparent anymore)
(defmethod instrument-form 'defn [n]
  (let [with-paths (cst/add-paths n)
        name (store/remark-insect! *ns* with-paths)
        zip (edn with-paths)
        def-sym-node (-> zip z/down z/node)
        def-sym (:value def-sym-node)
        name-sym-node (-> zip z/down z/right z/node)
        name-sym (:value name-sym-node)
        step-next (fn step-next [loc next-f pred]
                    ;; zips along next-f if (pred (z/node loc)) or stands still
                    (if (-> loc z/node pred)
                      (next-f loc)
                      loc))
        maybe-args (-> zip      ;; (defn ...)
                       z/down   ;; defn
                       z/right  ;; name
                       z/right
                       (step-next z/right string-node?)
                       (step-next z/right map-node?))
        first-args-loc (step-next maybe-args
                                  #(-> % z/right z/down)
                                  list-node?)
        multibodied? (list-node? (z/node maybe-args))
        loc (loop [xloc first-args-loc]
              (let [args-count (-> xloc z/node n/sexpr count)
                    zloc (-> (utils/instrument-form-with-args-and-body
                               xloc multibodied?)
                             (z/edit*
                               (fn [x]
                                 (-> x edn    ;; (scope-context ...)
                                     z/down   ;; scope-context
                                     z/right  ;; scope-paths
                                     z/right  ;; _some code_
                                     (z/edit*
                                       #(template
                                          `(handle-faked-recur
                                             ~'AAA
                                             ~'BBB)
                                          {:AAA args-count
                                           :BBB %}))
                                     z/up     ;; (scope-context ...)
                                     z/node))))
                    next-xloc (some-> zloc z/up z/right z/down)]
                (if next-xloc
                  (recur next-xloc)
                  zloc)))]
    (template
      `(context/with-insect-name
         ~name
         ~'AAA ;; TODO : uncomment ?
         ~'BBB
         ~'CCC)
      {:AAA (wrap-instrument (n/quote-node def-sym) def-sym-node)
       :BBB (wrap-instrument (n/quote-node name-sym) name-sym-node)
       :CCC (z/root loc)})))


;  _     _   _ .    |   [`       ,_   _
; _\ |) (/_ (_ | (| |   |  () |` ||| _\
;    |
(defn instrument-special-form [n]
  (let [sym-node (-> n edn z/down z/node)
        sym (:value sym-node)]
    (template
      '(do XXX YYY)
      {:XXX (wrap-instrument (n/quote-node sym) sym-node)
       :YYY (wrap-instrument (rewrite-form
                               n
                               identity
                               instrument))})))

(defmethod instrument-form :first-is-special [n]
  (instrument-special-form n))

; [` ,_   [`       ,_   _
; |  ||   |  () |` ||| _\
;                   
(defmethod instrument-form 'fn [n]
  (let [loc (-> (edn n) ;; converts to zipper
                 z/next  ;; fn
                 z/next) ;; name or args or ([params*] exprs*) +
        maybe-name (z/node loc)
        [name maybe-args] (if (token-node? maybe-name)
                            [(z/node loc) (z/next loc)]
                            [nil          loc])
        maybe-args-node (z/node maybe-args)
        multibodied? (list-node? maybe-args-node)
        first-args-loc (cond (vector-node? maybe-args-node) maybe-args
                              multibodied?                  (z/down maybe-args)
                              ;; TODO : keep this :else statement ?
                              :else (throw (ex-info (str "Invalid fn form "
                                                         (n/sexpr maybe-args))
                                                    {:type :invalid-fn-form
                                                     :node maybe-args})))
        loc (loop [xloc first-args-loc]
              (let [yloc (utils/instrument-form-with-args-and-body
                           xloc multibodied?)
                    next-xloc (some-> yloc z/up z/right z/down)]
                (if next-xloc
                  (recur next-xloc)
                  (if multibodied?
                    (-> yloc z/up z/up)
                    (-> yloc z/up)))))]
    (z/node loc)))

; [`       ,_   _       . |- |_   |  . ,_  | . ,_     _
; |  () |` ||| _\   LL| | |_ ||   |) | || (| | || (| _\
;                                                 _|
(defn instrument-binding-key [n]
  n)

(defn instrument-binding-val [n]
  (instrument n))

(defn instrument-bindings [loc]
  (loop [[end? l] [false (z/down loc)]] ;; left part of binding
    (if end?
      (-> l z/up)
      (recur
        (let [syms (utils/destructuring-exprs (z/node l))
              r (z/right l)] ;; right part of binding
          ;; add instrument code for the right part of the binding before the 
          ;; left part since the former can be redefined in the latter.
          (loop [xloc (z/edit* r instrument)
                 [s & ss] (reverse syms)]
            (if-not s
              (let [y (z/insert-right xloc (n/newline-node "\n"))]
                (if-let [z (z/right y)]
                  [false z]
                  [true y]))
              (recur
                (-> xloc
                    (z/insert-right (n/token-node (gensym "_")))
                    (z/insert-right (n/newline-node "\n")) ;; before gensym
                    z/right ;; gensym
                    (z/insert-right (wrap-instrument s))
                    z/right)
                ss))))))))
; |  _  |-   
; | (/_ |_ 
;
(defmethod instrument-form 'let [n]
  (let [inner-instr (loop [loc (-> (edn n) ;; converts to zipper
                                   z/down  ;; let
                                   z/next  ;; [bindings...]
                                   instrument-bindings)]
                      (if-let [new-loc (z/right loc)]
                        (-> (z/edit* new-loc instrument)
                            recur)
                        (-> loc z/up z/node)))
        let-sym-node (-> n edn z/down z/node)]
    (template
      '(do XXX YYY)
      {:XXX (wrap-instrument (n/quote-node 'let) let-sym-node)
       :YYY (wrap-instrument inner-instr)})))


#_(template
  `(loop ~'INSTRUMENTED_BINDINGS))
; |
; | () () |)
;         |
(defmethod instrument-form 'loop [n]
  (pprint (keys n))
  (let [bindings-loc (-> n edn
                         z/down   ;; loop
                         z/right) ;; [bindings ...]
        [destr-exprs bound-exprs]
        (if (empty? (-> bindings-loc z/node n/sexpr))
          [[] []]
          (loop [binding-expr (z/down bindings-loc) ;; first binding expr
                 dest-acc []
                 bound-acc []]
            (let [bound-expr (z/right binding-expr)
                  next-loc (z/right bound-expr)
                  new-dest-acc (->> binding-expr z/node
                                    utils/destructuring-exprs
                                    (concat dest-acc))
                  new-bound-acc (->> bound-expr z/node
                                     (conj bound-acc))]
              (if next-loc
                (recur next-loc new-dest-acc new-bound-acc)
                [new-dest-acc new-bound-acc]))))
        [loc body-nodes] (remove-rights bindings-loc)
        scope-paths (context/scope-paths destr-exprs)
        instr-bindings (concat (map wrap-instrument destr-exprs)
                               (map instrument bound-exprs))
        recur-args-count (-> bindings-loc
                             z/node
                             n/sexpr
                             count
                             (/ 2))
        loop-sym-node (-> n edn z/down z/node)
        loc (-> loc
                (z/insert-right
                  (template
                    `(handle-faked-recur
                       ~'XXX
                       (do
                         ~'YYY
                         ;; we'll put code here
                         ))
                    {:XXX recur-args-count
                     :YYY (wrap-instrument (n/quote-node 'loop)
                                           loop-sym-node)}))
                (z/insert-right (n/newline-node "\n"))
                z/right   ;; (handle-faked-recur ...)
                z/down    ;; handle-faked-recur
                z/right   ;; recur-args-count
                z/right   ;; (do ...)
                z/down    ;; do
                z/right)  ;; loop sym instrumenter
        instr-a (if (empty? instr-bindings)
                  loc
                  (loop [xloc loc
                         [b & more] instr-bindings]
                    (let [x-instr (-> xloc
                                      (z/insert-right b)
                                      (z/insert-right (n/newline-node "\n"))
                                      z/right)]
                      (if (empty? more)
                        x-instr
                        (recur x-instr more)))))
        instr-a (-> instr-a
                    (z/insert-right (n/newline-node "\n"))
                    (z/insert-right (template `(context/scope-context
                                                 ~'AAA)
                                              {:AAA scope-paths}))
                    z/right 
                    z/down
                    z/right)
        instr-b (if (empty? body-nodes)
                  (-> instr-a
                      (z/insert-right (n/newline-node "\n"))
                      (z/insert-right (n/token-node nil)))
                  (loop [xloc instr-a
                         [b & more] body-nodes]
                    (let [x-instr (-> xloc
                                      (z/insert-right (n/newline-node "\n"))
                                      (z/insert-right (instrument b)))]
                      (if (empty? more)
                        x-instr
                        (recur (z/right x-instr) more)))))
        instr-c (-> instr-b #_(adapt-recurs instr-b)
                    z/up ;; (do ...)
                    z/up ;; (handle-faked-recurs ...)
                    z/up ;; (scope-context ...)
                    z/up ;; (loop ...)
                    z/node
                    wrap-instrument)]
    instr-c))

;     _   _
; |` (/_ (_ L| |`
;
(defmethod instrument-form 'recur [n]
  (let [recur-sym-node (-> n edn z/down z/node)
        ret-sym (gensym "ret-")]
    (template
      `(do
         ~'XXX
         (let [~ret-sym ~'YYY]
           ~'ZZZ))
      {:XXX (wrap-instrument (n/quote-node 'recur) recur-sym-node)
       :YYY (rewrite-form
              n
              (constantly
                (n/token-node 'entomologist.instrument.recur/fake-recur))
              instrument)
       :ZZZ (wrap-instrument (n/token-node ret-sym) n)})))

;  |     _  _       ()   [`
; (| () _\ (/_ (|   (X   |  () |`
;               |

;; TODO : check that instrumentation order matches binding order in all the
;; concerned functions

;; TODO create functions such as scope-context, etc, and refactor accordingly.
(defn instrument-for-alike-form [n]
  (let [;; get bindings expr (not modifiers)
        bind-desctruct-exprs
        (->> n edn
             z/down   ;; (for ...)
             z/right  ;; [bindings...]
             z/node :children
             (remove
               #(or (not-code-node? %)
                    (some-> % :k #{:let :when :while})))
             (partition 2)
             (map first))
        ;; we only instrument destructuring 'tokens' (as opposed to, say,
        ;; destructuring vectors)
        bind-nodes-to-instrument
        (->> bind-desctruct-exprs
             n/vector-node
             utils/destructuring-exprs
             (filter token-node?)
             (group-by :string-value)
             (map (fn [[k [v]]]
                    [k (select-keys v [:value :path])]))
             (into {}))
        wrap-in-ctx (fn wrap-in-ctx [v]
                      (template `(context/scope-context
                                   ~'AAA
                                   ~'BBB)
                                {:AAA bind-nodes-to-instrument
                                 :BBB v}))
        ;; first we instrument the bindings
        instr-a
        (loop [bind-loc (-> n edn
                            z/down    ;; for
                            z/right   ;; [bindings...]
                            z/down)]  ;; first bind-sym
          (let [expr-loc (z/right bind-loc)
                instr-loc (let [k (some-> bind-loc z/node :k)]
                            (cond
                              (#{:let} k)
                              (loop [x-bind-loc (-> expr-loc instrument-bindings
                                                    z/down)]
                                (let [x-expr-loc (-> x-bind-loc z/right
                                                     (z/edit* wrap-in-ctx))
                                      next-loc  (z/right x-expr-loc)]
                                  (if next-loc
                                    (recur next-loc)
                                    (z/up x-expr-loc))))
                              (#{:while :when} k)
                              (-> expr-loc
                                  (z/edit* (comp wrap-in-ctx instrument)))
                              :else (z/edit* expr-loc instrument)))]
            (if-let [next-loc (z/right instr-loc)]
              (recur next-loc)
              (-> instr-loc
                   z/up     ;; [bindings...]
                   z/up)))) ;; (for ...)
        ;; then we wrap the body inside a scope context, instrumenting its
        ;; content beforehand
        body-loc (loop [xloc (-> instr-a
                                 z/down    ;; for
                                 z/right   ;; [bindings...]
                                 z/right)  ;; first in body
                        acc []];; we accumulate the body-exprs as we remove them
                   (let [new-acc (conj acc
                                       (instrument (z/node xloc))
                                       (n/newline-node "\n"))
                         removed-root-loc (-> xloc z/remove z/root edn)]
                     (if-let [next-loc (-> removed-root-loc
                                           z/down   ;; for
                                           z/right  ;; [bindings ..]
                                           z/right)];; first after args (or nil)
                       (recur next-loc new-acc)
                       ;; then we add them back wrapped inside a scope context
                       (-> removed-root-loc
                           z/down   ;; (for ...)
                           z/right  ;; [bindings ...]
                           (z/insert-right (n/newline-node "\n"))
                           (z/insert-right
                             (-> (n/list-node new-acc)
                                 edn z/down
                                 (z/insert-left (n/token-node 'do))
                                 z/left
                                 (z/insert-right (n/newline-node "\n"))
                                 z/up;; (do ...)
                                 z/node wrap-in-ctx))
                           z/right)))) ;; (scope-context ...)
        for-sym-node (-> n edn z/down z/node)
        for-sym (-> for-sym-node :value n/quote-node)
        instr-b
        (loop [xloc (-> body-loc
                        z/down  ;; scope-context
                        z/right ;; scope-paths
                        z/right ;; (do...)
                        z/down) ;; do
               [bind-node & more] (reverse bind-desctruct-exprs)]
          (let [instr-loc (-> xloc
                              (z/insert-right (n/newline-node "\n"))
                              (z/insert-right (wrap-instrument bind-node)))]
            (if more
              (recur instr-loc more)
              (-> instr-loc
                  (z/insert-right (n/newline-node "\n"))
                  (z/insert-right (wrap-instrument (n/quote-node for-sym)
                                                   for-sym-node))
                  z/root          ;; <FormsNode: (for ...)>
                  edn))))] ;; (for ...)
    (z/node instr-b)))

(defmethod instrument-form 'for [n]
  (instrument-for-alike-form n))

(defmethod instrument-form 'doseq [n]
  (instrument-for-alike-form n))

; |- |_     _      | . ,_      [`       ,_   _
; |_ || |` (/_ (| (| | || (|   |  () |` ||| _\
;                         _|
(defn instrument-threading-form [n]
  (let [instr-n
        (loop [loc (-> (edn n)  ;; converts to zipper
                       z/next)] ;; ->, or ->> etc ...
          (if-let [new-loc (z/right loc)]
            (let [;; instrument the contents of the threaded forms
                  ;; but not the forms themselves
                  l (z/edit* new-loc
                             (if (z/list? new-loc)
                               #(binding
                                  [*instrument-linear-coll-node?* false]
                                  (instrument %))
                               identity))
                  ;; then insert a form after this one to instrument its
                  ;; result                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    result
                  instr-sym (gensym "instr-")
                  m (z/insert-right
                      l (template `((fn [~'AAA]
                                      ~'BBB))
                                  {:AAA instr-sym
                                   :BBB (wrap-instrument-expr
                                          (n/token-node instr-sym)
                                          (z/node new-loc))}))]
              ;; skip the instrumenting form we've just added
              (recur (z/right m)))
            (-> loc z/up z/node)))
        thread-sym-node (-> n edn (z/get 0) z/node)
        thread-sym (:value thread-sym-node)]
    (template
      '(do XXX YYY)
      {:XXX (wrap-instrument (n/quote-node 'thread-sym) thread-sym-node)
       :YYY instr-n})))

(defmethod instrument-form '-> [n]
  (instrument-threading-form n))

(defmethod instrument-form '->> [n]
  (instrument-threading-form n))

; [`    |-        _       |  _  |                    ,_  .  _  _
; |  L| |_ L| |` (/_ ,   (| (/_ | (| \/ ,   |) |` () ||| | _\ (/_
;                                    /      |
(defn instrument-ipending [n]
  (let [x (-> n edn z/down z/node)
        ret-sym (gensym "ret-")]
    (wrap-instrument-expr
      (template `(do (when (resolve '~(:value x))
                       (instrument-runtime *ns*
                                           ~context/insect-name-sym
                                           ~(:path x)
                                           ~context/context-sym
                                           (symbol-info '~(:value x))))
                     (let [~ret-sym ~'XXX]
                       #_(future
                         (loop []
                           (Thread/sleep 200)
                           (if (realized? ~ret-sym)
                             ~'YYY
                             (recur))))
                       ~ret-sym))
                {:XXX (rewrite-form n identity instrument)
                 :YYY (wrap-instrument (n/token-node ret-sym) n)})
      n)))

(defmethod instrument-form 'future [n]
  (instrument-ipending n))

(defmethod instrument-form 'delay [n]
  (instrument-ipending n))

(defmethod instrument-form 'promise [n]
  (instrument-ipending n))

;  _     _  _
; (_ (| _\ (/_
;

;; REPRENDRE : exception ligne 710 : prendre en compte le cas par défaut
;; TODO : handle multiple test-constants
(defmethod instrument-form 'case [n]
  (let [case-sym-node (-> n edn z/down z/node)
        case-sym (:value case-sym-node)
        [instr-a test-constants]
        (loop [xloc (-> n edn
                        z/down  ;; case
                        z/right ;; tested expr
                        (z/edit* instrument)
                        z/right);; first test-constant
               acc []]
          (if-let [yloc (z/right xloc)]
            (let [zloc (-> xloc z/right
                           (z/edit* instrument))
                  new-acc (conj acc (z/node xloc))]
              (if-let [new-xloc (z/right zloc)] ;; next test-constant (maybe)
                (recur new-xloc new-acc)
                [(z/up zloc) new-acc]))
            ;; if there is nothing to the right of xloc, then we've reached
            ;; the default result expr
            [(z/edit* xloc instrument)
             acc]))
        instr-b (loop [xloc (-> (n/list-node '(do))
                                edn z/down)
                       [tc & more] test-constants]
                  (let [yloc (-> xloc
                                 (z/insert-right (wrap-instrument tc))
                                 z/right)]
                    (if (empty? more)
                      yloc
                      (recur yloc more))))
        instr-c (-> instr-b
                    (z/insert-right (z/node instr-a))
                    (z/insert-right (n/newline-node "\n"))
                    z/up)
        ret-sym (gensym "ret-")]
    (println "case-sym :" case-sym)
    (println "case-sym-node :" case-sym-node)
    (template `(do ~'XXX
                   (let [~ret-sym ~'YYY]
                     ~'ZZZ))
              {:XXX (wrap-instrument (n/quote-node case-sym) case-sym-node)
               :YYY (z/node instr-c)
               :ZZZ (wrap-instrument (n/token-node ret-sym) n)})))







