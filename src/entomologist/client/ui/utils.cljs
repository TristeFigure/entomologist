(ns entomologist.client.ui.utils
  (:require [reagent.core :as reagent]))

(def should-update?
  (reagent/atom true))

(defmacro inhibiting-updates [& body]
  `(do
     (reset! should-update? false)
     ~@body
     (reset! should-update? true)))