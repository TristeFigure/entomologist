(ns entomologist.client.core
  (:use [cljs.pprint :only [pprint]])
  (:require [reagent.core :as reagent]
            [entomologist.client.router :refer [start-router!]]
            [entomologist.client.store :as store]
            [entomologist.client.test :as test]
            [entomologist.client.ui.main :refer [main test-main]]))

(enable-console-print!)

; (test/run)



(declare handle-push-message)

(defmulti receive-push-message :id)

(defmethod receive-push-message :default [{:keys [id event]}]
  (println "Unhandled message" id event))

(defmethod receive-push-message :chsk/handshake [{:keys [?data]}]
  (println "Handshake :" ?data))

(defmethod receive-push-message :chsk/state [{:keys [?data]}]
  (if (= ?data {:first-open? true})
    (println "Channel socket successfully established")
    (println "Channel socket state changed :" ?data)))

(defmethod receive-push-message :chsk/recv [{:keys [id event ?data]}]
  ; (println "Push event from server :" ?data)
  (handle-push-message ?data))


(defmulti handle-push-message first)

(defmethod handle-push-message :ento/multimessage [[_ msgs]]
  (println "count multimessage :" (count msgs)))

(defmethod handle-push-message :ento/set-all-insects! [[_ all-data]]
  (println "SETTING ALL")
  (println "--------------------------------------")
  (store/set-all-insects! all-data))

(defmethod handle-push-message :ento/set-insect! [[_ data]]
  (println "SETTING INSECT")
  (store/set-insect! (:insect/namespace data)
                     (:insect/name data)
                     (:insect/cst data)))

(defmethod handle-push-message :ento/delete-insect! [[_ data]]
  (println "DELETING INSECT")
  (store/delete-insect! (:insect/namespace data)
                        (:insect/name data)))

(defmethod handle-push-message :ento/set-observation! [[_ data]]
  (println "SETTING OBSERVATION")
  (store/set-observation! (:insect/namespace data)
                          (:insect/name data)
                          (:observation/path data)
                          (:observation/context data)
                          (:observation/value data)))

(defmethod handle-push-message :ento/delete-observation! [[_ data]]
  (println "DELETING OBSERVATION")
  (store/delete-observation! (:insect/namespace data)
                             (:insect/name data)
                             (:observation/path data)
                             (:observation/context data)))

(defmethod handle-push-message :ento/set-cst! [[_ data]]
  (println "SETTING CST")
  (store/set-cst! (:insect/namespace data)
                  (:insect/name data)
                  (:insect/cst data)))


(defn push-message-handler [{:keys [id event] :as msg}]
  (println "received message" id event)
  (receive-push-message msg))


(start-router! push-message-handler)

;; TODO : remove
(defn fig-reload []
 (println "----------------------------------")
 (println "Figwheel reloaded !"))




(defn app []
  #_[:div [main]]
  [:div [test-main]])

(defn mount-app! []
  (reagent/render-component [app]
                            (.getElementById js/document "app")))

(def _ (mount-app!))
