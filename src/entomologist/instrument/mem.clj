(ns entomologist.instrument.mem
  (:import [clojure.lang IPersistentCollection
                         IFn])
  (:require [clojure.pprint :refer [pprint]]))

(defprotocol MemP
  (value [this])
  (update [this f]
          [this f & args])
  (flat [this])
  (memupdate [this f & args])
  (replay [this]
          [this other]))

(deftype PrettyMemory [pretty f]
  IFn
  (invoke [this m]
    (f m))
  
  Object
  (toString [this m]
    (with-out-str
      (pprint pretty))))

(deftype Mem [m #^IPersistentCollection mem initial-state]
  MemP
  (value
    [this]
    m)
  
  (update
    [this f]
    (Mem. (f m) (.conj mem f) initial-state))
  
  (flat
    [this]
    (Mem. m (.emtpy mem) m))
  
  (memupdate
    [this f & args]
    (Mem. m
          (apply f mem args)
          initial-state))
  
  (replay
    [this]
    (replay this this))
  
  (replay
    [this other]
    (if (instance? Mem other)
      (let [f-all (apply comp (reverse (.seq mem)))
            res (f-all (.m other))]
        (Mem. res
              (.empty (.mem other))
              (.initial-state other)))))
  
  
  Object
  (toString
    [this]
    (.to-string m)))

(defmethod print-method Mem [^Mem x ^Writer w]
  (print-method
    (summary-as-vec (.flushed-cnt x) (.start-buffer x) (.end-buffer x))
    w))

(defmethod print-dup Mem [^Mem x ^Writer w]
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




(defn make-mem
  ([m]
   (make-mem m []))
  ([m mem]
   (make-mem m mem m))
  ([m mem initial-state]
   (Mem. m mem initial-state)))
