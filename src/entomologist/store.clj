(ns entomologist.store
  (:use [clojure.pprint])
  (:require [rewrite-clj.node :as n]
            [entomologist.cst :as cst]
            [entomologist.config :refer [config]]
            [entomologist.event :refer [event!]]
            [entomologist.rewrite-clj.complements :refer [def-form-node?]]
            [entomologist.client-server.protocols.limited :refer [limited?]]
            [entomologist.client-server.limited-map
             :refer [limited-map]
             :reload true])
  (:import [clojure.lang APersistentMap]))

;; TODO: really deduce name argument from CST ? Should we not process the
;;       magnifying glass form in order to get arbitrary insect IDs ? See TODO
;;       in entomologist.store

;; TODO: make an agent instead and get rid of the agent async ?
(def ^:private *previous-store-type
  (atom {}))

(defmacro def-store [name type]
  (let [type (keyword type)
        db_   (gensym "db-")
        args_ (gensym "args-")]
    (when true #_(not= type @*previous-store-type)
      (swap! *previous-store-type assoc name type)
      (case type
        :atom   `(do
                   (def ~name
                     (atom {}))

                   (defmacro store-read [~db_]
                     ~``(deref ~~db_))

                   (defmacro store-write! [~db_ & ~args_]
                     ~``(swap! ~~db_ ~@~args_)))
        :ref    `(do
                   (def ~name
                     (ref {}))

                   (defmacro store-read [~db_]
                     ~``(deref ~~db_))

                   (defmacro store-write! [~db_ & ~args_]
                     ~``(dosync (alter ~~db_ ~@~args_))))
        :agent  `(do
                   (def ~name
                     (agent {}))

                   (defmacro ~'store-read [~db_]
                     ~``(deref ~~db_))

                   (defmacro ~'store-write! [~db_ & ~args_]
                     ~``(send ~~db_ ~@~args_)))))))

(def-store db :agent)

;; TODO: make watch plugin-ready.
;; IDEA: create a 'on' macro that'd act very much like javascript event emitter
;;       method, but that would'd also allow to specifiy order constraints.
; (add-watch
;   db ::test-watch
;   (fn [key reference old-state new-state]
;     (println "watch !")))


(defn differ-as-sexprs?
  "Returns true if the nodes' sexprs differ."
  [node-a node-b]
  (and node-a node-b
       (not= (n/sexpr node-a) (n/sexpr node-b))))

(defn differ-as-strings?
  "Return true if the nodes' string differ."
  [node-a node-b]
  (and node-a node-b
       (not= (n/string node-a) (n/string node-b))))

(defn insect-type [cst]
  (if (def-form-node? cst)
    :def-form
    :anonymous-form))

;; TODO: the 'name' of an anonymous insect should be a generated ID placed in a
;;       comment, something like `;; -o/19827`.
;;       Actually, the -o macro should be able to take a succession of pairs of
;;       key value pairs where a key is a keyword with namespace '-o'.
(defn insect-name
  ([cst]
   (insect-name (insect-type cst) cst))
  ([type cst]
   (case type
     :def-form         (-> cst n/sexpr second str)
     :anonymous-form   (str (java.util.UUID/randomUUID)))))

(defn get-insect [ns name]
  (get-in (store-read db) [ns name]))

(defn insect-exists? [ns name]
  (boolean (get-insect ns name)))

(defn create-insect!
  ([ns cst]
   (create-insect! ns (insect-name cst) cst))
  ([ns name cst]
   (store-write! db assoc-in [ns name]
             {:insect/cst cst})
   (event! [:insect-created! {:insect/namespace ns
                              :insect/name name
                              :insect/cst cst}])))

(defn renew-insect!
  "Drop any observation associated with this insect."
  ([ns cst]
   (renew-insect! ns (insect-name cst) cst))
  ([ns name cst]
   ;; speed matters. As per create-insect! implementation, any
   ;; observation is dropped (since the new cst is not the same).
   (create-insect! ns name cst)))

(defn update-insect-cst!
  ([ns cst]
   (update-insect-cst! ns (insect-name cst) cst))
  ([ns name cst]
   (let [stored-cst (:insect/cst (get-insect ns name))]
     (if (differ-as-sexprs? cst stored-cst)
       (renew-insect! ns name cst)
       (when (differ-as-strings? cst stored-cst)
         (store-write! db assoc-in [ns name :insect/cst] cst)
         (event! [:insect-cst-updated! {:insect/namespace ns
                                        :insect/name name
                                        :insect/cst cst}]))))))

(defn upsert-insect!
  ([ns cst]
   (upsert-insect! ns (insect-name cst) cst))
  ([ns name cst]
   (if (insect-exists? ns name)
     (update-insect-cst! ns name cst)
     (create-insect! ns name cst))))

(defn remark-insect! [ns cst]
  (let [name (insect-name cst)]
    (->> cst
         cst/clean-node
         (upsert-insect! ns name))
    name))

(def limited-observations-keys
  #{:runtime})

(defn- limited-assoc-in [m [k & ks] v]
  (if ks
    (assoc m k
      (let [x (get m k {})
            xx (if (contains? limited-observations-keys k)
                 (if-not (limited? x)
                   (limited-map (:observations-limit @config) x)
                   x)
                 x)]
        (limited-assoc-in xx ks v)))
    (assoc m k v)))

(defn set-observation! [ns name cst-pth ctx value]
  (let [db-keys (vec (concat [ns name :insect/observations] cst-pth ctx))]
    (store-write! db (fn [db-val]
                   (limited-assoc-in db-val db-keys value)))))

; To make set-observation more modular / plugin-ready
; (def assoc-in-rules
;   (atom {}))

; (declare assoc-in-observations)

; (defn default-assoc-in-observation-rule [m ks v]
;   )

; (defn get-observation-rule [k]
;   (or (@assoc-in-rules k)
;       default-assoc-in-observation-rule))

; (defn assoc-in-observations-rules [m [k & ks] v]
;   (if ks
;     (let [f (get-observation-rule k)]
;       (assoc m k (f (get m k) ks v)))
;     (assoc m k v)))

; (defn observation-rule [k f]
;   (swap! assoc-in-observations-rules
;          assoc k f))

; (observation-rule
;   :exec
;   (fn [m ks v]
;     (let [mm (if (limited-seq? m)
;                m
;                (limited-seq))])
;     (assoc-in-observations mm ks v)))

(defn deep-first [x]
  (if (coll? x)
    (let [f (first x)]
      (deep-first f))
    x))

;; TODO: remove
; (defn reach** [m [k & more-ks]]
;   (mapcat (fn [[kk v]]
;             (if (= kk k)
;               (if (empty? more-ks)
;                 [v]
;                 (if (associative? v)
;                   (reach** v more-ks)
;                   []))
;               []))
;           m))

; (defn reach* [m [k & more-ks :as ks]]
;   (mapcat (fn [[kk v]]
;             (if (= k kk)
;               (if (empty? more-ks)
;                 [v]
;                 (reach** v more-ks))
;               (if (associative? v)
;                 (reach* v ks)
;                 [])))
;           m))

; (defn reach [m k-or-ks]
;   (reach* m (if (sequential? k-or-ks)
;               k-or-ks
;               [k-or-ks])))

(defn time-mark-after?
  "(time-mark-after? x x) returns true."
  [[a aa] [b bb]]
  (cond
    (.before a b) false
    (.after a b) true
    :else (>= aa bb)))

(declare get-observations)

(defmulti observation-transform
  #(deep-first (first %&)))

(defmethod observation-transform :execs [_ obs [ns insect-name cst-path ctx]]
  (->> (get-in obs [:runtime :exec])
       (mapcat (fn [[k v]]
                 (map (fn [[kk vv]]
                        {:value vv
                         :time-mark kk
                         :scope (->> k
                                     (map (fn [scp-pth]
                                            [scp-pth
                                             (first
                                               (get-observations
                                                 [ns insect-name scp-pth []]
                                                 [[:value-at kk]]))]))
                                     (into {}))})
                      v)))
       (sort-by :time-mark)))

(defmethod observation-transform :value-at [[_ time-mark] obs
                                            [ns insect-name cst-path ctx]]
  (->> (get-in obs [:runtime :exec])
       vals
       (apply concat)
       (sort-by first)
       (split-with (fn [[tm v]]
                     (time-mark-after? time-mark tm)))
       first
       last
       second))

(defn get-observations
  ([ref]
   (get-observations ref []))
  ([[ns insect-name cst-path ctx :as ref] transforms]
    (let [path (concat [ns insect-name :insect/observations] cst-path ctx)
          ret (get-in (store-read db) path)]
      (if (sequential? transforms)
        (if (empty? transforms)
          ret
          (map #(observation-transform % ret ref)
               transforms))
        (if (nil? transforms)
          ret
          (observation-transform transforms ret ref))))))


(defn testit [ns]
  (pprint (get-observations [ns "toto" [2 2 2 1] []]
                            :execs)))

(defn testit2 [ns]
  (pprint (get-observations [ns "toto" [2 2 1 0] []]
                            :execs)))


