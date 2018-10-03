(ns entomologist.explorations.datascript
  (:require [datascript.core :as d]))


(def datascript-db
  (d/create-conn {}))


(defn datascript-create-datum! []
  (d/transact! datascript-db [{:db/id -1
                               :datum/value :xxx}]))




(def ref-last-id
  (atom 0))

(defn ref-gen-id []
  (swap! ref-last-id inc))

(def ref-db
  (ref {}))

(defn ref-create-datum! []
  (dosync (alter ref-db assoc (ref-gen-id) {:datum/value :xxx})))



(def atom-last-id
  (atom 0))

(defn atom-gen-id []
  (swap! atom-last-id inc))

(def atom-db
  (atom {}))

(defn atom-create-datum! []
  (swap! atom-db assoc (atom-gen-id) {:datum/value :xxx}))



(time
  (dotimes [n 100000]
    (atom-create-datum!)))
;; "Elapsed time: 477.058 msecs"

(time
  (dotimes [n 100000]
    (ref-create-datum!)))
;; "Elapsed time: 7084.56 msecs"

(time
  (dotimes [n 100000]
    (datascript-create-datum!)))
;; "Elapsed time: 36906.138 msecs"


;; --> CONCLUSION
;;     Datascript is way too slow