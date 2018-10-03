(ns entomologist.instrument.static
  (:use clojure.pprint)
  (:require [rewrite-clj.node :as n]
            [entomologist.rewrite-clj.complements :refer [def-form-node?]]))

(defn instrument-static [n]
  n)