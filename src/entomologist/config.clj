(ns entomologist.config
  (:require [clojure.pprint :refer [pprint]]))

(def config-file
  (clojure.java.io/file ".entomologist-cfg.edn"))

(def default-config
  {:doall true
   :observations-limit 50})

(defonce config
  (let [a (atom (if (.exists config-file)
                  (read-string (slurp config-file))
                  (do (spit config-file
                            (with-out-str (pprint default-config)))
                      default-config)))]
    (add-watch a :entomologist
               (fn [_k _ref _old-v new-v]
                 (spit config-file
                       (with-out-str (pprint new-v)))))
    a))