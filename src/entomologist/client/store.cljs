(ns entomologist.client.store
  (:use [cljs.pprint :only [pprint]])
  (:require [reagent.core :as reagent]
            [entomologist.client.ui.utils :refer [inhibiting-updates]]))

(defonce state
  (reagent/atom {}))


(defn set-insect! [ns name cst]
  (swap! state update-in [:insects ns name]
         assoc
         :cst cst))

(defn delete-insect! [ns name]
  (swap! state update-in [:insects ns]
         dissoc name))

;; TODO make sure any observation is deleted when a new one is set
(defn set-observation! [ns name pth ctx value]
  (swap! state assoc-in [:insects ns name :observations pth ctx]
         value))

(defn set-all-insects! [all-data]
  ;; TODO : wrap around inhibiting-updates ?
  (doseq [d all-data
          :let [ns (:insect/namespace d)
                name (:insect/name d)
                cst (:insect/cst d)
                obs (:insect/observation d)]]
    (set-insect! ns name cst)
    (doseq [[pth m] obs]
      (doseq [[ctx value] m]
        (set-observation! ns name pth ctx value))))
  (println "All insects set !"))

(defn delete-observation! [ns name pth ctx]
  (println ctx)
  (swap! state update-in [:insects ns name :observations pth]
         dissoc ctx)
  (println "Observation deleted !"))

(defn set-cst! [ns name cst]
  (swap! state assoc-in [:insects ns name :cst] cst))

(defn get-insects-list []
  (->> @state :insects
       (mapcat (fn [[ns m]]
                 (map (fn [[name {:keys [cst]}]]
                        [ns name cst])
                      m)))))

(defn get-namespaces []
  (->> @state :insects
       (map key)
       distinct
       sort))

(defn set-last-hovered-path! [pth]
  (swap! state assoc :last-hovered-path pth))

(defn get-last-hovered-path []
  (:last-hovered-path @state))	

(defn get-contextualized-values [ns insect-name path]
  (get-in @state [:insects ns insect-name :observations path]))

(defn not-code-node? [node]
  (#{:comment-node :newline-node :whitespace-node :uneval-node}
     (:class node)))

(defn cst-get-at-path [cst [p & more :as pth]]
  (let [cs (remove not-code-node? (:children cst))]
    (if more
      (recur (nth cs p)
             more)
      (if (empty? pth)
        cs
        (nth cs p)))))

(defn get-cst-node [ns insect-name path]
  (let [cst (get-in @state [:insects ns insect-name :cst])]
    (cst-get-at-path cst path)))

; (defn get-debug []
;   (for [[ns m] (get-in @state [:insects])]
;     (map (fn [x] [ns x])
;          (keys m))))




