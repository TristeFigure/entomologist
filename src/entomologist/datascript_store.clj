;; TOO DAMN SLOW

(ns entomologist.datascript-store
  (:use clojure.pprint)
  (:require [rewrite-clj.zip.base :refer [edn]]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [datascript.core :as d]
            [entomologist.cst :as cst]
            [entomologist.rewrite-clj.complements :refer [def-form-node?]]
            [entomologist.event :refer [event!]
             :reload true]
            [entomologist.server.core :as server]
            [entomologist.server.projection :refer [project-insect]
             :reload true]
            [entomologist.server.router :as router]
            [entomologist.instrument.context :refer [subcontext?]]))

(defn ensure-ffirst [c]
  (assert (= 1 (count c)))
  (ffirst c))

(defn ensure-first [c]
  (assert (= 1 (count c)))
  (first c))

(def schema
  {:insect/observation  {:db/cardinality :db.cardinality/many
                         :db/valueType :db.type/ref
                         :db/isComponent true}})

(defonce db
  (d/create-conn schema))

; TODO : remove
(defn reset-state! []
  (def db (d/create-conn schema)))



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

(defn insect-name [ns cst]
  (case (insect-type cst)
    :def-form         (-> cst n/sexpr second str)
    :anonymous-form   (-> cst n/sexpr str)))

(declare get-insect)

(defn insect-exists? [ns name]
  (not (empty? (d/q '[:find  ?e
                      :in $ ?ns ?name
                      :where
                      [?e :insect/namespace ?ns]
                      [?e :insect/name ?name]]
                    @db ns name))))

(defn ensure-insect-exists [ns name]
  (when-not (insect-exists? ns name)
    (throw (ex-info (str "Insect not found for namespace " (str ns)
                         " and name " name)
                    {:type :insect-not-found
                     :namespace ns
                     :name name}))))

(defn ensure-insect-doesnt-exist [ns name]
  (when (insect-exists? ns name)
    (throw
      (ex-info (str "Insect already exists for namespace "(str ns)
                    " and name " name)
               {:type :insect-already-exists
                :namespace ns
                :name name
                :insect (get-insect ns name)}))))

(defn create-insect! [ns cst]
  (let [type (insect-type cst)
        name (insect-name ns cst)]
    (ensure-insect-doesnt-exist ns name)
    (d/transact! db [{:db/id -1
                      :insect/type type
                      :insect/namespace ns
                      :insect/name name
                      :insect/cst cst}])
    (event! [:insect-created! {:insect/namespace ns
                               :insect/name name
                               :insect/cst cst}])))

(defn get-insect [ns name]
  (ensure-insect-exists ns name)
  (ensure-ffirst
    (d/q '[:find  (pull ?e [*])
           :in $ ?ns ?name
           :where
           [?e :insect/namespace ?ns]
           [?e :insect/name ?name]]
         @db ns name)))

(defn get-all-insects []
  (->> (d/q '[:find (pull ?e [*])
              :where [?e :insect/name _]]
            @db)
       (map first)))

(defn delete-insect! [ns name]
  (let [id (:db/id (get-insect ns name))]
    (d/transact! db [[:db.fn/retractEntity id]])
    (event! [:insect-deleted! {:insect/namespace ns
                               :insect/name name}])))

(defn renew-insect! [ns name cst]
  (let [cst-name (insect-name ns cst)]
    (when (not= cst-name name)
      (throw (ex-info (str "Tried to renew insect "
                           "(" name ")"
                           " with a cst that yields a different name"
                           " (" cst-name ")")
                      {:type :renew-insect-name-mismatch
                       :name name
                       :cst-name cst-name
                       :cst cst})))
    (delete-insect! ns name)
    (create-insect! ns cst)))

(defn ensure-observation-doesnt-exist [ns name pth ctx]
  (let [x (d/q '[:find ?o
                 :in $ ?namespace ?name ?pth ?ctx
                 :where
                 [?i :insect/namespace ?namespace]
                 [?i :insect/name ?name]
                 [?i :insect/observation ?o]
                 [?o :observation/path ?pth]
                 [?o :observation/context ?ctx]]
               @db
               ns name pth ctx)]
    (when-not (empty? x)
      (throw
        (ex-info
          (str "Observation already exists for insect with namespace " ns
               " and name " name " in context " (vec ctx) " at path " pth)
          {:type :observation-already-exists
           :ns ns
           :name name
           :ctx ctx})))))

(defn create-observation! [ns name pth ctx value]
  (ensure-observation-doesnt-exist ns name pth ctx)
  (ensure-insect-exists ns name)
  (let [insect-id (:db/id (get-insect ns name))]
    (d/transact! db [{:db/id -1
                      :observation/path pth
                      ;; Dirty hack around the impossibility to store nil
                      ;; in datascript attributes.
                      ;; See entomologist.client-server.displayable
                      :observation/value (if (nil? value)
                                           ::nil
                                           value)}
                     [:db/add -1 :observation/context ctx]
                     [:db/add insect-id :insect/observation -1]])
    (event! [:observation-created!
             {:insect/namespace ns
              :insect/name name
              :observation/path pth
              :observation/context ctx
              :observation/value value}])))

;; TODO : write test for this
(defn delete-observation! [ns name pth ctx]
  (let [id (ensure-ffirst (d/q '[:find ?o
                                 :in $ ?ns ?n ?pth ?ctx
                                 :where
                                 [?i :insect/namespace ?ns]
                                 [?i :insect/name ?n]
                                 [?i :insect/observation ?o]
                                 [?o :observation/path ?pth]
                                 [?o :observation/context ?ctx]]
                               @db ns name pth ctx))]
    (d/transact! db [[:db.fn/retractEntity id]])
    (event! [:observation-deleted! {:insect/namespace ns
                                    :insect/name name
                                    :observation/path pth
                                    :observation/context ctx}])))

(defn set-observation! [ns name pth ctx value]
  (create-observation! ns name pth ctx value)
  #_(let [obs-ctxs (d/q '[:find ?pth ?ctx
                        :in $ ?ns ?n
                        :where
                        [?i :insect/namespace ?ns]
                        [?i :insect/name ?n]
                        [?i :insect/observation ?o]
                        [?o :observation/context ?ctx]
                        [?o :observation/path ?pth]]
                      @db ns name)
        kill-list (->> obs-ctxs
                       (remove #(subcontext? ctx (last %))))]
    (doseq [[pth ctx] kill-list]
      (delete-observation! ns name pth ctx))
    (create-observation! ns name pth ctx value)))

(defn update-insect-cst! [ns name cst]
  (let [i (get-insect ns name)
        stored-cst (:insect/cst i)
        insect-id (:db/id i)]
    (if (differ-as-sexprs? cst stored-cst)
      (renew-insect! ns name cst)
      (when (differ-as-strings? cst stored-cst)
        (d/transact db [[:db/add insect-id :insect/cst cst]])
        (event! [:insect-cst-updated! {:insect/namespace ns
                                       :insect/name name
                                       :insect/cst cst}])))))

(defn upsert-insect! [ns cst]
  (let [name (insect-name ns cst)]
    (if (insect-exists? ns name)
      (update-insect-cst! ns name cst)
      (create-insect! ns cst))))

(defn remark-insect! [ns cst]
  (let [name (insect-name ns cst)]
    (->> cst
         cst/clean-node
         (upsert-insect! ns))
    name))

(defn mark-exec! [ns insect-name pth ctx value]
  (set-observation! ns insect-name pth ctx value))

;  /|  _    ,_  _
; /-| _\ \/ || (_
;        /
; (defonce mark-agent
;   (agent [] :error-handler (fn [ag ex]
;                              (println "-------- agent")
;                              (pprint ag)
;                              (println "-------- stacktrace")
;                              (.printStackTrace ex))))

; (defn async-mark! [& call]
;   (send mark-agent conj call))

; (defn flush-mark-agent! []
;   (send-off mark-agent (fn [ag]
;                          (doseq [[name & args] ag]
;                            (if (= name :remark-insect!)
;                              (apply remark-insect! args)
;                              (apply mark-exec! args)))
;                          [])))

; (defonce agent-flusher-running?
;   (atom false))

; (defn stop-mark-agent-flusher! []
;   (when @agent-flusher-running?
;     (reset! agent-flusher-running? false)))

; (defn start-mark-agent-flusher! []
;   (when @agent-flusher-running?
;     (stop-mark-agent-flusher!)
;     (Thread/sleep 200))
;   (reset! agent-flusher-running? true)
;   (future
;     (loop []
;       (Thread/sleep 200)
;       (flush-mark-agent!)
;       (when @agent-flusher-running?
;         (recur)))))

; (defonce _
;   (start-mark-agent-flusher!))

; (defn async-remark-insect! [ns cst]
;   (async-mark! :remark-insect! ns cst))

; (defn async-mark-exec! [ns insect-name pth ctx value]
;   (async-mark! :mark-exec! ns insect-name pth ctx value))




;; can't place this in server.core or server.push because of cyclical
;; dependencies
(defn push-all! [uid]
  (println "Pushing all to client with uid" uid)
  (let [all-data (->> (get-all-insects)
                      (map project-insect))]
    (router/chsk-send! uid [:ento/set-all-insects! all-data])))

(defmethod server/receive-message :chsk/uidport-open [{:keys [uid]}]
  (println "Client connected with uid" uid)
  (push-all! uid))






