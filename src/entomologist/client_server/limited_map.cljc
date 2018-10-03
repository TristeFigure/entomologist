(ns entomologist.client-server.limited-map
  (:use clojure.pprint)
  (:require [entomologist.client-server.protocols.limited :refer [LimitedP]])
  (:import [clojure.lang IPersistentMap
                         Associative]
           [java.io Writer]))

(deftype LimitedMap
  [^int limit ^boolean past-limit ^IPersistentMap delegate]
  LimitedP
  (limit [this]
    limit)
  (limited? [this]
    true)
  (past-limit? [this]
    past-limit)
  
  IPersistentMap
  (assoc [this k v]
    (if (= (.limit this) (.count delegate))
      (LimitedMap. limit true delegate)
      (LimitedMap. limit false (.assoc delegate k v))))
  (assocEx [this k v]
    (.assocEx this k v))
  (without [this k]
    (LimitedMap. limit past-limit (.without delegate k)))
  
  ;; - IPersistentCollection
  (count [this]
    (.count delegate))
  (cons [this x]
    (if (= (.limit this) (.count delegate))
      (LimitedMap. limit true delegate)
      (LimitedMap. limit false (.cons (.seq this) x))))
  (empty [this]
    (LimitedMap. limit (zero? limit) []))
  (equiv [this other]
    (if (instance? LimitedMap other)
      (and (= limit (.limit other))
           (.equiv delegate (.delegate other)))
      (.equiv delegate (.delegate other))))
  (seq [this]
    (.seq delegate))
  
  ;; - ILookup
  (valAt [this v else-v]
    (.valAt delegate v else-v))
  (valAt [this v]
    (.valAt delegate v))
  
  ;; - Counted
  ; (count [this]
  ;   (.count delegate))
  
  ;; - Iterable
  (iterator [this]
    (.iterator delegate))
  
  Object
  (toString [this]
    (str delegate)))

(defmethod print-method LimitedMap [^LimitedMap x ^Writer w]
  (print-method
    (.delegate x)
    w)
  (when (.past-limit? x)
    (.write w "...")))

(defmethod print-dup LimitedMap [^LimitedMap x ^Writer w]
  (.write w "#limited-map ")
  (print-method [`~(symbol (str (.limit x)))
                 (.delegate x)]
                w))

(defn limited-map
  ([limit]
   (limited-map limit []))
  ([limit ^Associative s]
   (let [s (if (instance? LimitedMap s)
             (.delegate s)
             s)]
     (if-not limit
        (LimitedMap. (count s) false s)
        (LimitedMap. limit
                     (> (.count s) limit)
                     (into (empty s) (take limit s)))))))

(defn limited-map* [args]
  (apply limited-map args))
