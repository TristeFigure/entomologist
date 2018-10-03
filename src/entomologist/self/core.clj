(ns entomologist.self.core
  (:use clojure.pprint)
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [rewrite-clj.zip.base :refer [edn]]
            [entomologist.rewrite-clj.complements :refer [rights]
             :reload true]
            [entomologist.self.read :refer [get-form-text raw-code]
             :reload true]
            [entomologist.self.util :refer [ns-file]
             :reload true]))

;; TODO: remove dependency on raw-code

(let [raw (raw-code
            (do (+ 1 2 3)))]
  (pprint raw)
  (println
    (binding [*print-dup* true]
      (pr-str raw))))
(throw (Exception. "Spiderman"))
(println "* * * * * * * * * * * * * * * * * * *")

(defn self-body [nodes]
  (-> nodes
      edn       ;; (self :macro [& args])
      z/down    ;; self
      z/right   ;; :macro
      z/right   ;; [& args]
      rights))  ;; body

(defmacro self
  "Example:
   • (self :macro [raw])
   • "
   [strategy & more]
   (case strategy
     :macro (let [[name [this-sym & more-args :as args] & body] more
                  {:keys [line column]} (meta &form)
                  f (ns-file *ns*)
                  form-text (get-form-text f line column)
                  __ (gensym "_-")]
              `(let [~this-sym  (self-body (p/parse-string ~form-text))]
                 (defmacro ~name ~(vec more-args)
                   ~@body)))))

(defmacro def-self-macro [name [this-sym & more-args :as args] & body]
  (let [{:keys [line column]} (meta &form)
        f (ns-file *ns*)
        form-text (get-form-text f line column)
        __ (gensym "_-")]
    `(let [~this-sym  (self-body (p/parse-string ~form-text))]
       (defmacro ~name ~(vec more-args)
         ~@body))))


(pprint (macroexpand-1 '(def-self-macro toto [nodes x y z]
                         `[:abc
                           ~nodes])))

(println " - - - - - - - - - - ")

(def-self-macro toto [nodes x y z]
  [:abc
    nodes])

(pprint (macroexpand-1 '(toto 1 2 3)))

(throw (Exception. "mince"))
(self :macro truc [this x y z]
      (+ x y z))
(println (truc 1 2 3))

#_(self :macro toto [this x y z]
      [1 2 3 [:a :b :c]]
      :abc
      (inc 3))
