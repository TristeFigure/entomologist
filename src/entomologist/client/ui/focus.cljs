(ns entomologist.client.ui.focus
  (:require [entomologist.client.store :as store]))

;; reference means : [ns insect-name path]

;; soft focus on hovered node
(defn set-soft-focus! [reference]
  (swap! store/state assoc :soft-focus reference))

(defn get-soft-focus []
  (:soft-focus @store/state))

(defn unset-soft-focus! []
  (set-soft-focus! nil))


;; hard focus on last clicked node
(defn set-hard-focus! [reference]
  (swap! store/state assoc :hard-focus reference))

(defn get-hard-focus []
  (:hard-focus @store/state))

(defn unset-hard-focus! []
  (set-hard-focus! nil))


;; soft focus has precedence over hard focus
(defn get-focus []
  (if-let [s (get-soft-focus)]
    s
    (get-hard-focus)))

