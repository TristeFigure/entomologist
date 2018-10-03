(ns entomologist.self.write
  (:use clojure.pprint)
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [entomologist.self.util :refer [ns-file navigate-line-col]
             :reload true]
            [entomologist.self.read :refer [raw-code]]
            [entomologist.rewrite-clj.complements
             :refer [rewrite-clj-node?]]))

(defmacro replace-self [replacement]
  (let [{:keys [line column]} (meta &form)
        f (ns-file *ns*)]
    ;; we use edn* so as to begin at the very start, whitespaces included
    `(let [f# (ns-file *ns*)
           loc# (navigate-line-col
                  (z/edn* (p/parse-file-all f#))
                  ~line ~column)]
       (-> (if (rewrite-clj-node? ~replacement)
             (z/replace* loc# ~replacement)
             (z/replace loc# ~replacement))
            z/root-string
            (->> (spit f#))))))

(defmacro def-self-replace [name replacement]
  (let [f-sym (gensym "f-")
        loc-sym (gensym "loc-")
        gencode (fn [l c]
                  `(let [f# (ns-file *ns*)
                         loc# (navigate-line-col
                                (z/edn* (p/parse-file-all f#))
                                ~l ~c)]
                     (-> (if (rewrite-clj-node? ~replacement)
                           (z/replace* loc# ~replacement)
                           (z/replace loc# ~replacement))
                         z/root-string
                         (->> (spit f#)))))]
    `(defmacro ~name []
       (let [{:keys [~'line ~'column]} (meta ~'&form)]
         ~(gencode 'line 'column)))))

(defmacro def-self-edit [name edit-fn]
  (let [f-sym (gensym "f-")
        loc-sym (gensym "loc-")
        gencode (fn [l c]
                  `(let [f# (ns-file *ns*)
                         loc# (navigate-line-col
                                (z/edn* (p/parse-file-all f#))
                                ~l ~c)]
                     (-> (z/edit* loc# ~edit-fn)
                         z/root-string
                         (->> (spit f#)))))]
    `(defmacro ~name []
       (let [{:keys [~'line ~'column]} (meta ~'&form)]
         ~(gencode 'line 'column)))))

(defmacro def-self-edit-sexpr [name edit-sexpr-fn]
  (let [f-sym (gensym "f-")
        loc-sym (gensym "loc-")
        gencode (fn [l c]
                  `(let [f# (ns-file *ns*)
                         loc# (navigate-line-col
                                (z/edn* (p/parse-file-all f#))
                                ~l ~c)]
                     (-> (z/edit loc# ~edit-sexpr-fn)
                         z/root-string
                         (->> (spit f#)))))]
    `(defmacro ~name []
       (let [{:keys [~'line ~'column]} (meta ~'&form)]
         ~(gencode 'line 'column)))))


(def-self-replace abc :xyz)

(def-self-edit wrap-in-vector!
  (fn [n]
    (n/vector-node [n])))

(def-self-edit-sexpr cons-toto
  (fn [sexpr]
    (cons :toto sexpr)))



; (abc)

; (wrap-in-vector!)

; (cons-toto)

