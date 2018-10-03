(ns entomologist.core
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

; (store/reset-state!) ;; TODO : removed so remove this


;; TODO: document doall behavior / or find somethin else than thread bindings
;; to pass insect-name/exec-context down to mark-exec .


(defmacro -xo [& args]
  `(do ~@args))

(defmacro -ø [& args]
  `(do ~@args))

; (-o (let [a 12
;           b 24
;           d :dd e :ee]
;       (if (> a b)
;         d e)))

; (-o (map (fn [x y]
;            (map (fn [z]
;                   (inc z))
;                 (range x)))
;          (range 3)
;          (range 3 6)))


; (-o (def toto
;       (map (fn [x]
;              (map (fn [y]
;                     (map (fn [z]
;                            (inc z))
;                          (range y)))
;                   (range x)))
;            (range 9))))

;; TODO deal with static java fields and methods as well as the . & .. macros
(defn sleep [n]
  (Thread/sleep n))

; (-o (defn toto [a]
;       (map (fn [x]
;              (* x 2))
;            (range 10))))

; (-o (defn fib
;       ([n]
;        (fib [0 1] n))
;       ([x n]
;        (if (< (count x) n)
;          (fib (conj x (+ (last x) (nth x (- (count x) 2)))) n)
;          x))))

; (-o (def titi
;       (case 456
;         123 :abc
;         456 :def
;         :drefault)))

;; TODO : investigate var-args with recur
(comment
  (defn recursive2 [& args]
    (println "args : " args)
    (let [x (first args)]
      (if (zero? x)
        x
        (recur (dec x))))))

; (-o (def toto
;       (loop [x 0]
;         (if (= x 3)
;           x
;           (recur (inc x))))))


; (defn toto [x]
;   (recur x))

; (-o (map (fn [x]
;            (map (fn [y]
;                   (map inc (range y)))
;                 (range x)))
;          (range 10)))


; (-o (def toto
;       "ahahaha
;       ohohoh"))

; (-o (def toto
;       ((fn [x] (inc x))
;        33)))

; (-o (def zaza
;       (let [x 123]
;         (inc x))))

; ((fn [x]
;    (println x)
;    x))



(defmacro -o [& _]
  (let [{:keys [line column]} (meta &form)
        f (ns-file *ns*)
        form-text (get-form-text f line column)
        nodes (p/parse-string form-text)
        instrumented (instrument nodes)]
    (n/sexpr instrumented)))

;; 10 000  | 16    | 4180 |
;; 1 000   | 6     | 442
;; 100     | 5     | 58
;; 10      | 5     | 15
(-o (def toto
      (do :abc
          (loop [x 3]
            (if (zero? x)
              x
              (recur (dec x)))))))
; (loop [x 1]
;   (if (= x 1000000)
;     :done
;     (do (println "x =" x)
;         (time (-o (loop [y x]
;                     (if (zero? y)
;                       y
;                       (recur (dec y))))))
;         (recur (* 10 x)))))

;; TODO : resolve bug when appending #_ in front of -o. Or is the bug caused
;; by the special setting of the dev environment : that is with the following
;; let ?

