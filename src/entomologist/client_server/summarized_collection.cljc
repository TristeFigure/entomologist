(ns entomologist.client-server.summarized-coll
  (:require [clojure.pprint :refer [pprint simple-dispatch]])
  (:import [clojure.lang IPersistentCollection
                         PersistentVector
                         PersistentQueue
                         ISeq]
           [java.io Writer]))
;        _      _
; (| L| (/_ L| (/_
;  |
;; SEE : src/data_readers.clj
;; no need to do this for clojurescript as it already has the #queue data-reader
(defn queue
  ([]
   PersistentQueue/EMPTY)
  ([xs]
   (into PersistentQueue/EMPTY xs)))

(defmethod print-method PersistentQueue [^PersistentQueue x ^Writer w]
  (.write w "#queue ")
  (print-method (.seq x) w))

;  _    ,_  ,_        . _   _   |    _    | |  _   _ |- .    ,_
; _\ L| ||| ||| (| |` | /_ (/_ (|   (_ () | | (/_ (_ |_ | () ||
;

;; TODO : reader-macro for clojurescript
;; (reader/register-tag-parser! 'foo identity)
;; more info @ http://stackoverflow.com/questions/31430489/can-reader-tags-be-used-with-clojurescript
(defn summary-as-vec [flushed-cnt start-buffer end-buffer]
  (vec (concat start-buffer
               [['.. flushed-cnt 'skipped '..]]
               end-buffer)))

(deftype SummarizedColl [^int start-limit ^int end-limit
                               ^int flushed-cnt
                               ^PersistentVector start-buffer
                               ^PersistentQueue  end-buffer]
  IPersistentCollection
  (count [this]
    (+ (count start-buffer)
       flushed-cnt
       (count end-buffer)))
  (cons [this x]
    (if (< (count start-buffer) start-limit)
      (SummarizedColl. start-limit end-limit flushed-cnt
                             (.cons start-buffer x) end-buffer)
      (if (< (count end-buffer) end-limit)
        (SummarizedColl. start-limit end-limit flushed-cnt
                               start-buffer (.cons end-buffer x))
        (SummarizedColl. start-limit end-limit (inc flushed-cnt)
                               start-buffer (-> end-buffer
                                                pop
                                                (.cons x))))))
  (empty [this]
         (SummarizedColl. start-limit end-limit 0
                                [] (queue)))
  (equiv [this other]
    (if (instance? SummarizedColl other)
      (and (.equiv start-buffer (.start-buffer other))
           (.equiv end-buffer (.end-buffer other))
           (= flushed-cnt (.flushed-cnt other)))
      (and (sequential? other)
               (zero? flushed-cnt)
               (.equiv (.seq this) other))))
  (seq [this]
    (concat start-buffer end-buffer))
  
  Object
  (toString [this]
    (str (summary-as-vec flushed-cnt start-buffer end-buffer))))



(defmethod print-method SummarizedColl [^SummarizedColl x ^Writer w]
  (print-method
    (summary-as-vec (.flushed-cnt x) (.start-buffer x) (.end-buffer x))
    w))

(defmethod print-dup SummarizedColl [^SummarizedColl x ^Writer w]
  (.write w "#summarized-coll ")
  (print-method [`~(symbol (str (.start-limit x)))
                 `~(symbol (str (.end-limit x)))
                 `~(symbol (str (.flushed-cnt x)))
                 (.start-buffer x) (.end-buffer x)]
                w))

(.addMethod
  simple-dispatch SummarizedColl
  (fn [x]
    (pprint (summary-as-vec (.flushed-cnt x) (.start-buffer x) (.end-buffer x)))))

(defn summarized-coll
  ([limit]
   (summarized-coll limit []))
  ([start-limit end-limit flushed-cnt
    ^PersistentVector start-buffer ^PersistentQueue end-buffer]
   (SummarizedColl. start-limit end-limit flushed-cnt
                          start-buffer end-buffer))
  ([limit ^ISeq s]
   (let [half (/ limit 2)
         start-limit (int (Math/ceil half))
         end-limit (int (Math/floor half))]
     (reduce (fn [^SummarizedColl acc x] (.cons acc x))
             (summarized-coll start-limit end-limit 0 [] (queue))
             s))))

(defn summarized-coll* [args]
  (apply summarized-coll args))
