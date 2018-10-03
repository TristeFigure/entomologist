(ns entomologist.instrument.runtime
  (:use [clojure.pprint :only [pprint]])
  (:require [rewrite-clj.node :as n]
            [entomologist.store :as store]))

;; TODO: remove this file and make the db an agent

(defonce db-requests
  (agent nil))

(defn set-observation* [_ ns insect-name path execution-context value]
  (store/set-observation! ns insect-name path execution-context value))

(defn async-mark-exec! [ns insect-name path execution-context value]
  (send db-requests set-observation* ns insect-name path execution-context value))

;; TODO : what to do with imbricated def forms ?
;; (def name1 ... (def name 2 ...) ...)
;; TODO : do defs inside let forms actually work ?
(defn instrument-runtime [ns insect-name path execution-context value]
  ; (println "---------ec-----------" path)
  ; (pprint execution-context)
  #_(async-mark-exec! ns insect-name path execution-context value)
  (store/set-observation! ns insect-name path execution-context value))