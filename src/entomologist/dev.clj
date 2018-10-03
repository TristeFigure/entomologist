(ns entomologist.dev
  (:use clojure.pprint)
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]
            [cljfmt.core :refer [reformat-string]]
            [entomologist.self.read :refer [get-form-text]
             :reload true]
            [entomologist.self.util :refer [ns-file]
             :reload true]
            [entomologist.instrument.core :refer [instrument]
             :reload true]
            [entomologist.store :as store
             :reload true]
            [entomologist.server.core :as server
             :reload true]))

(defonce _ (server/start!))

(defmacro -o [& _]
  (let [{:keys [line column]} (meta &form)
        f (ns-file *ns*)
        form-text (get-form-text f line column)
        nodes (p/parse-string form-text)
        instrumented (instrument nodes)]
    (println line column)
    (def lin line)
    (def col column)
    (n/sexpr instrumented)))

(defn remove-keyword-namespaces [cst]
  (clojure.walk/postwalk (fn [x]
                           (if (symbol? x)
                             (symbol (name x))
                             x))
                         cst))

(defn print-n-eval [sexpr print?]
  (when print?
    (-> sexpr
        ; clojure.walk/macroexpand-all
        ; remove-keyword-namespaces
        (clojure.pprint/write :dispatch clojure.pprint/code-dispatch)
        with-out-str
        reformat-string
        println))
  (time (println (eval sexpr))))

(defn reset-state! []
  (store/db-write! store/db (constantly {})))

(defn run
  ([]
   (run false))
  ([print?]
   (println "--------------------------------")
   (reset-state!)
   (let [f (ns-file *ns*)
         form-text (get-form-text f lin col)
         node (p/parse-string form-text)
         instrumented (instrument node)]
     (-> instrumented
         n/sexpr
         (print-n-eval print?)))))




(-o (def toto
         (do :abc
             (loop [x 3]
               (if (zero? x)
                 x
                 (recur (dec x)))))))


(run)
(println "==============================")
(store/testit *ns*)
(println "==============================")
(store/testit2 *ns*)



