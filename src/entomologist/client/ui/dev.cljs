(ns entomologist.client.ui.dev
  (:require [reagent.core :as reagent] ))

(defn display-guard [guard-name component]
  (let [displayed? (reagent/atom false)]
    (fn []
      [:div
       [:span guard-name
        " ||| display?"
        [:input {:type "checkbox"
                 :checked @displayed?
                 :on-change (fn [_e] (swap! displayed? not))}]]
       (when @displayed?
         (if (fn? component)
           [component]
           component))])))