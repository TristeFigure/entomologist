(ns entomologist.cst
  (:require [cljfmt.core :as cljfmt]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [rewrite-clj.zip.base :refer [edn]]
            [entomologist.rewrite-clj.complements
             :refer [not-code-node? uneval-node? seq-node? quote-node?
                     rewrite-clj-node?]]))

(defn add-children-paths [node]
  (assert (seq-node? node))
  (let [cs (:children node)
        node-pth (:path node)]
    (->> cs
         (reductions (fn [[i n] x]
                       [(if (and (not-code-node? x)
                                 (not (uneval-node? x)))
                          i (inc i))
                        x])
                     [-1 nil])
         rest
         (map (fn [[i n]]
                (if (and (not-code-node? n)
                         (not (uneval-node? n)))
                  n
                  (assoc n :path (conj node-pth i)))))
         (assoc node :children))))

(defn add-paths* [node]
  (if (and (seq-node? node)
           (not (quote-node? node)))
    (-> node
        add-children-paths
        (update-in [:children] (partial map add-paths*)))
    node))

(defn add-paths [node]
  (add-paths* (assoc node :path [])))

(defn update-at-path* [loc [k & ks :as pth] f & args]
  (assert (-> loc z/node n/inner?))
  (if-let [moc (z/get loc k)]
    (if ks
      (apply update-at-path* moc ks f args)
      (z/edit* moc #(apply f % args)))
    false))

(defn update-at-path [n pth f & args]
  (if (empty? pth)
    n
    (if-let [x (apply update-at-path* (edn n) pth f args)]
      (z/root x)
      (throw (ex-info (str "Invalid path " pth)
                      {:type :inexistant-path
                       :path pth
                       :node n})))))

(defn cst-at-path [cst [p & pth]]
  (if (empty? pth)
    p
    (recur (nth (:children cst)) pth)))

(defn clean-node [node]
  (-> node
      cljfmt/reformat-form
      edn
      z/node))

