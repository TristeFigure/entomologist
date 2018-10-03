(ns entomologist.instrument.symbol-info
  "see entomologist.instrument.wrap/wrap-instrument-symbol*")

(defrecord VarInfo [file ns name arglist doc])

(defrecord SpecialFormInfo [forms doc])

(def special-doc-map
  @#'clojure.repl/special-doc-map)

(defn symbol-info [s]
  (assert (symbol? s))
  (if-let [{:keys [forms doc]} (special-doc-map s)]
    (map->SpecialFormInfo {:forms forms :doc doc})
    (when-let [v (resolve s)]
      (map->VarInfo (select-keys (meta v)
                                 [:ns :file :ns :name :arglists :doc])))))