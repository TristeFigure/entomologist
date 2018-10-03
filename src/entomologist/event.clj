(ns entomologist.event
  (:require [entomologist.server.push :as push
             :reload true]))

(def ^:dynamic *should-event?*
  true)

(defmacro inhibiting-events [& body]
  `(binding [*should-event?* false]
     ~@body))

(defn event! [[name data]]
  (when *should-event?*
    (case name
      :insect-created!      (push/push-insect! data)
      :insect-deleted!      (push/push-delete-insect! data)
      :insect-renewed!      (push/push-insect! data)
      :insect-cst-updated!  (push/push-cst! data)
      :observation-created! (push/push-observation! data)
      :observation-deleted! (push/push-delete-observation! data))))