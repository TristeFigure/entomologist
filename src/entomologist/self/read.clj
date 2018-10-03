(ns entomologist.self.read
  (:require [clojure.java.io :as io]
            [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]
            [entomologist.rewrite-clj.complements :refer [not-code-node?]]
            [entomologist.self.util :refer [ns-file]])
  (:import [java.io PushbackReader]))


(defn get-form-text [f line col]
  (with-open [rdr (io/reader f)]
    (->> (line-seq rdr)
         (drop (dec line))
         ((fn [[f & r]]
            (cons (->> f  (drop (dec col))  (apply str))
                  r)))
         (interpose [\newline])
         (reduce concat)
         (split-with (let [cnt (atom 0)]
                       #(do (case %
                              \( (swap! cnt inc)
                              \) (swap! cnt dec)
                              nil)
                            (not (zero? @cnt)))))
         ((fn [[a b]]  (concat a [(first b)])))
         (reduce str)
         doall)))

(defmacro raw-code
  "Macro that reads its own expression from the file it is
  contained in and returns its content as rewrite-clj parse
  data."
  [& _]
  (let [{:keys [line column]} (meta &form)
        f (if-let [f (ns-file *ns*)]
            f
            (throw (ex-info (str "raw-code : no file found for namespace " *ns*)
                            {:type :namespace-file-not-found
                             :namespace *ns*})))
        form-text (get-form-text f line column)
        parsed (p/parse-string form-text)
        content (->> parsed
                     :children
                     (drop-while not-code-node?)
                     (drop 1);; removes initial 'raw-code'
                     )
        contentx (remove not-code-node? content)]
    (if (= 1 (count contentx))
      (first contentx)
      (n/forms-node (drop-while not-code-node? content)))))