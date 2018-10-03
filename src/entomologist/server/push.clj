(ns entomologist.server.push
  (:use clojure.pprint)
  (:require [entomologist.server.router :as router]
            [entomologist.server.projection :refer [adapt-for-client]]))

; ~|~ |_  _     _   _  _  _  ,_ |- .    |
;  |  || (/_   (/_ _\ _\ (/_ || |_ | (| |
;
(defn push! [msg]
  (doseq [uid (:any @router/connected-uids)]
    (router/chsk-send! uid msg)))

;  /|  _    ,_  _
; /-| _\ \/ || (_
;        /
; (defonce push-agent
;   (agent '() :error-handler (fn [ag ex]
;                               (println "-------- agent")
;                               (pprint ag)
;                               (println "-------- stacktrace")
;                               (.printStackTrace ex))))

; (defn async-push! [msg]
;   (send push-agent #(cons msg %)))

; (defn flush-push-agent! []
;   (send-off push-agent (fn [msgs]
;                          (push! [:ento/multimessage msgs])
;                          '())))

; (defonce agent-flusher-running?
;   (atom false))

; (defn stop-push-agent-flusher! []
;   (when @agent-flusher-running?
;     (reset! agent-flusher-running? false)))

; (defn start-push-agent-flusher! []
;   (when @agent-flusher-running?
;     (stop-push-agent-flusher!)
;     (Thread/sleep 200))
;   (reset! agent-flusher-running? true)
;   (future
;     (loop []
;       (Thread/sleep 200)
;       (flush-push-agent!)
;       (when @agent-flusher-running?
;         (recur)))))

; (defonce __
;   (entomologist.server.push/start-push-agent-flusher!))


; |)    |  | .  _   [` ,_  _
; |  L| |) | | (_   |  || _\
;
(defn push-insect! [data]
  (push! [:ento/set-insect! (doall (adapt-for-client data))]))

(defn push-delete-insect! [data]
  (push! [:ento/delete-insect! (doall (adapt-for-client data))]))

(defn push-cst! [data]
  (push! [:ento/set-cst! (doall (adapt-for-client data))]))

(defn push-observation! [data]
  (push! [:ento/set-observation! (doall (adapt-for-client data))]))

(defn push-delete-observation! [data]
  (push! [:ento/delete-observation! (doall (adapt-for-client data))]))
