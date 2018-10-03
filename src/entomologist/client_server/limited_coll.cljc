(ns entomologist.client-server.limited-coll
  (:require [entomologist.client-server.protocols.limited :refer [LimitedP]])
  (:import [clojure.lang IPersistentCollection
                         ISeq]
           [java.io Writer]))

(deftype LimitedColl
  [^int limit ^boolean past-limit ^IPersistentCollection delegate]
  LimitedP
  (limit [this]
    limit)
  (limited? [this]
    true)
  (past-limit? [this]
    past-limit)
  
  IPersistentCollection
  (count [this]
    (.count delegate))
  (cons [this x]
    (if (= limit (.count delegate))
      (LimitedColl. limit true delegate)
      (LimitedColl. limit false (.cons delegate x))))
  (empty [this]
    (LimitedColl. limit (zero? limit) []))
  (equiv [this other]
    (if (instance? LimitedColl other)
      (and (= limit (.limit other))
           (.equiv delegate (.delegate other)))
      (.equiv delegate (.delegate other))))
  (seq [this]
    (.seq delegate))
  
  Object
  (toString [this]
    (str delegate)))

(defmethod print-method LimitedColl [^LimitedColl x ^Writer w]
  (print-method
    (vec (concat (.delegate x)
                 (when (.past-limit? x)
                   '[...])))
    w))

(defmethod print-dup LimitedColl [^LimitedColl x ^Writer w]
  (.write w "#limited-coll ")
  (print-method [`~(symbol (str (.limit x)))
                 (.delegate x)]
                w))

(defn limited-coll
  ([limit]
   (limited-coll limit []))
  ([limit ^ISeq s]
   (let [s (if (instance? LimitedColl s)
             (.delegate s)
             s)]
     (if-not limit
       (LimitedColl. (count s) false s)
       (LimitedColl. limit
                     (> (.count s) limit)
                     (into (empty s) (take limit s)))))))

(defn limited-coll* [args]
  (apply limited-coll args))
