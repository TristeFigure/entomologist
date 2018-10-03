(ns entomologist.client-server.protocols.limited)

(defprotocol LimitedP
  (limit [this])
  (limited? [this])
  (past-limit? [this]))

(extend-type Object
  LimitedP
  (limit [this]
    nil)
  (limited? [this]
    false)
  (past-limit? [this]
    nil))