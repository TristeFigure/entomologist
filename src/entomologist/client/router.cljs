(ns entomologist.client.router
  (:require [taoensso.sente :as sente]))

; copy of entomologist.server.router
; TODO : do the necessary cljx/cljc refactoring

; TODO : place the string "/chsk" in entomologist.params
(defonce _
  (let [{:keys [chsk ch-recv send-fn state]}
        (sente/make-channel-socket! "/chsk" ; Note the same path as before
                                    {:type :auto ; e/o #{:auto :ajax :ws}
                                     })]
    (def chsk       chsk)
    (def ch-chsk    ch-recv) ; ChannelSocket's receive channel
    (def chsk-send! send-fn) ; ChannelSocket's send API fn
    (def chsk-state state)   ; Watchable, read-only atom
    ))

(defonce stop-router-fn
  (atom nil))

(defn  stop-router! []
  (when-let [stop-f @stop-router-fn]
    (stop-f)))

(defn start-router! [msg-handler]
  (stop-router!)
  (reset! stop-router-fn
          (sente/start-chsk-router! ch-chsk msg-handler)))