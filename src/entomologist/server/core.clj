(ns entomologist.server.core
  (:require [org.httpkit.server :refer [run-server]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.middleware.reload :as reload]
            [ring.middleware.json :refer [wrap-json-response]]
            [ring.util.response :refer [response]]
            [compojure.handler :refer [api]]
            [compojure.route :as route]
            [compojure.core :refer [defroutes GET POST PUT]]
            [hiccup.core :refer [html]]
            
            [entomologist.server.params :as params]
            [entomologist.server.router :refer [start-router! stop-router!]
             :as router]))

(defn index []
  (html
    [:html
     [:head
      [:link {:rel "stylesheet" :type "text/css"
              :href "css/main.css"}]
      [:link {:rel "stylesheet" :type "text/css"
              :href "https://fonts.googleapis.com/css?family=Oxygen+Mono"}]
      [:link {:rel "stylesheet" :type "text/css"
              :href "re-com/css/bootstrap.css"}]
      [:link {:rel "stylesheet" :type "text/css"
              :href "re-com/css/re-com.css"}]
      [:link {:rel "stylesheet" :type "text/css"
              :href "re-com/css/bootstrap.css"}]
      [:link {:rel "stylesheet" :type "text/css"
              :href "re-com/css/material-design-iconic-font.min.css"}]
      [:link {:rel "stylesheet" :type "text/css"
              :href "http://fonts.googleapis.com/css?family=Roboto:300,400,500,700,400italic"}]
      [:link {:rel "stylesheet" :type "text/css"
              :href "http://fonts.googleapis.com/css?family=Roboto+Condensed:400,300"}]]
     [:body
      [:div#app]]
     [:script {:type "text/javascript" :src "js/main.js"}]]))

(defroutes all-routes
  (GET "/" [] (index))

  (GET  "/chsk" req (router/ring-ajax-get-or-ws-handshake req))
  (POST "/chsk" req (router/ring-ajax-post req))
  
  (route/not-found "<p>Page not found.</p>")) ;; all other, return 404


(defn build-site [routes]
  (-> (api routes) ;; includes wrap-keyword-params and wrap-params needed by taoensso.sente
      (wrap-resource "public")
      wrap-json-response))

(defn run []
  (let [handler (if params/in-dev?
                  (reload/wrap-reload (build-site #'all-routes)) ;; only reload when dev
                  (build-site all-routes))]
    (run-server handler {:port params/port})))



(defmulti receive-message :id)

(defmethod receive-message :default [{:keys [id event]}]
  (println "Unhandled message" id event))



(defmethod receive-message :chsk/ws-ping [_]
  ;; do nothing
  )

; (defmethod receive-message :chsk/handshake [{:keys [?data]}]
;   (println "Handshake :" ?data))

; (defmethod receive-message :chsk/state [{:keys [?data]}]
;   (if (= ?data {:first-open? true})
;     (println "Channel socket successfully established")
;     (println "Channel socket state changed :" ?data)))

; (defmethod receive-message :chsk/recv [{:keys [id event ?data]}]
;   (println "Push event from server :" ?data)
  ; (handle-push-message ?data))

(defn message-handler [{:keys [id event] :as msg}]
  ; (println "received message" id event)
  (receive-message msg))


(defonce stop-current (atom nil))

(defn stop-server! []
  (when @stop-current
    (@stop-current)
    (reset! stop-current nil)))

(defn start-server! []
  (stop-server!)
  (reset! stop-current (run)))

(defn stop! []
  (stop-server!)
  (stop-router!))

(defn start! []
  (stop!)
  (start-router! message-handler)
  (start-server!))
