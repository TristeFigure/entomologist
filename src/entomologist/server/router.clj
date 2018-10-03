(ns entomologist.server.router
  (:require [taoensso.sente :as sente]
            [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]))

(defonce _
  (let [{:keys [ch-recv send-fn connected-uids
                ajax-post-fn ajax-get-or-ws-handshake-fn
                connected-uids]}
        (sente/make-channel-socket! (get-sch-adapter)
                                    {:user-id-fn :client-id})]
    (def ring-ajax-post                ajax-post-fn)
    (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
    (def ch-chsk                       ch-recv) ; ChannelSocket's receive channel
    (def chsk-send!                    send-fn) ; ChannelSocket's send API fn
    (def connected-uids                connected-uids) ; Watchable, read-only atom
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


