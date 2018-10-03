(ns entomologist.mem-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [entomologist.instrument.mem
             :refer [make-mem]
             :reload true]))

(deftest test-make-mem
  (let [m (make-mem {})]
    (update m ()))
 #_(is (= (make-mem m)))
 #_(is (= ))
 #_(is (= )))

(deftest test-mem
  )

(deftest test-memflat
  )

(deftest test-replay
  )

(deftest test-memupdate
  )

; (defprotcol MemP
;   (mem [this f])
;   (memflat [this])
;   (replay [this])
;   (replay [this other])
;   (memupdate [this f & args]))

(run-tests)