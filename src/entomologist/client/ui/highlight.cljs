(ns entomologist.client.ui.highlight
  (:require [entomologist.client.store :as store]))

(defn set-highlight! [reference value]
  (swap! store/state update-in [:highlights reference]
         (fnil conj #{}) value))

(defn remove-highlight! [reference value]
  (let [new-val (disj (get-in @store/state [:highlights reference])
                      value)]
    (if (empty? new-val)
      (swap! store/state update-in [:highlights]
             dissoc reference)
      (swap! store/state update-in [:highlights reference]
             disj value))))

(defn get-highlights [reference]
  (get-in @store/state [:highlights reference]))

(defn get-all-highlights []
  (:highlights @store/state))