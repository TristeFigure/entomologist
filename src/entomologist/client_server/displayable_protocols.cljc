(ns entomologist.client-server.displayable-protocols)

(defprotocol Displayed
  (displayed [this]))

(defprotocol Displayable
  (display [this]))