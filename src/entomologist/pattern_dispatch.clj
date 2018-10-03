(ns entomologist.pattern-dispatch
  (:use [clojure.pprint])
  (:require [clojure.core.match :refer [match]]))

(def pd-store
  (atom {}))

(defmacro defpd [name]
  (swap! pd-store assoc-in [name]
         {:cases []})
  nil)

(defn compile-pd [name]
  (let [cases (->> (get-in @pd-store [name :cases])
                   (mapcat (juxt :pattern
                              (fn [x] `(do ~@(:body x))))))
        in-sym (gensym "in-")]
    `(fn [x#]
       (println "----" x#)
       (match [x#]
              ~@cases
              :else ::no-match))))

(defmacro defmatch [pd-name pattern & body]
  (assert (contains? @pd-store pd-name)
          (str "First define a pattern-dispatch  for "
               pd-name " with (defpd " pd-name " ...)"))
  (swap! pd-store update-in [pd-name :cases] conj
         {:pattern pattern  :body body})
  `(def ~pd-name
     ~(compile-pd pd-name)))


; (def toto
;   {:name "toto"
;    :gender :male})

; (def tata
;   {:name "tata"
;    :gender :female})

; (defpd test-pd)

; (defmatch test-pd [{:name "toto"}]
;   (println "a name is toto !"))

; (defmatch test-pd [{:gender :female}]
;   (println "a gender is female !"))

; (test-pd toto)
; (test-pd tata)


