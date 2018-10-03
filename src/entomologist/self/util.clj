(ns entomologist.self.util
  (:require [clojure.java.io :as io]
            [clojure.tools.namespace.find
             :refer [find-clojure-sources-in-dir]]
            [clojure.tools.namespace.file
             :refer [read-file-ns-decl]]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [entomologist.rewrite-clj.complements
             :refer [newline-node? string-node? comment-node? set-node?]
             :reload true])
  (:import [rewrite_clj.node.comment CommentNode]
           [rewrite_clj.node.forms FormsNode]
           [rewrite_clj.node.integer IntNode]
           [rewrite_clj.node.keyword KeywordNode]
           [rewrite_clj.node.meta MetaNode]
           [rewrite_clj.node.quote QuoteNode]
           [rewrite_clj.node.reader_macro
            ReaderNode ReaderMacroNode DerefNode]
           [rewrite_clj.node.seq SeqNode]
           [rewrite_clj.node.string StringNode]
           [rewrite_clj.node.token TokenNode]
           [rewrite_clj.node.uneval UnevalNode]
           [rewrite_clj.node.whitespace WhitespaceNode NewlineNode]))

(defn ns-file [ns]
  (let [m (->> (find-clojure-sources-in-dir (io/file "."))
               (map #(let [decl (read-file-ns-decl %)
                           name (second decl)]
                       [name %]))
               (into {}))]
    (get m (ns-name ns))))

(defn count-newlines [s]
  (count (filter (partial = \newline)  s)))

(defn navigate-line [loc line]
  (loop [xloc loc
         nlines 1]
    (let [next-xloc (z/next* xloc) ;; next* because we want to zip over
          node (z/node xloc)       ;; whitespaces as well
          new-nlines (+ nlines
                        (if (or (newline-node? node)
                                (string-node? node)
                                (comment-node? node))
                          (count-newlines (n/string node))
                          0))]
      
      
      (if (z/end? xloc)
        (if (> line nlines)
          (throw (ex-info "Line index out of bounds"
                          {:type :line-index-out-of-bounds
                           :loc loc  :line line}))
          xloc)
        (if (= line nlines)
          xloc
          (recur next-xloc new-nlines))))))

(defn navigate-col [loc col]
  (loop [xloc loc
         nchars 1]
    (let [yloc (z/next* xloc)
          node (z/node xloc)
          cls (class node)
          [next-xloc adv]
          (cond
            (#{FormsNode} cls)                  [yloc 0]
            (#{StringNode} cls)                 [yloc 2]
            (#{ReaderMacroNode DerefNode} cls)  [yloc 1]            
            (#{WhitespaceNode} cls)     [yloc (-> node :whitespace count)]
            (#{CommentNode} cls)        [yloc (-> node :s count inc)]
            (#{IntNode TokenNode} cls)  [yloc (-> node :value str count)]
            (#{KeywordNode} cls)        [yloc (-> node :k str count)]
            
            (#{MetaNode QuoteNode ReaderNode} cls)
            [yloc (-> node :prefix count)]
            
            (#{UnevalNode} cls)
            [yloc (+ 2 (-> node :children first :format-string
                           (clojure.string/replace #"%s" "")
                           count))]
            
            (#{SeqNode} cls)
            (let [cnt (+ nchars (count (n/string node)))]
              (if (<= cnt col)
                [(z/right* xloc) cnt]
                [yloc (if (set-node? node)
                        2 1)]))
            :else (throw (ex-info "Column index out of bounds"
                                  {:type :column-index-out-of-bounds
                                   :loc loc  :column col})))
          new-nchars (+ nchars adv)]
      (if (z/end? xloc)
        (if (> col nchars)
          (throw (ex-info "Column index out of bounds"
                          {:type :column-index-out-of-bounds
                           :loc loc  :column col}))
          xloc)
        (if (= col nchars)
          xloc
          (recur next-xloc new-nchars))))))

(defn navigate-line-col [loc line column]
  (println "navigating :" line column)
  (-> loc
      (navigate-line line)
      (navigate-col column)))

;; TODO: do something about this
;; https://github.com/xsc/rewrite-clj/issues/38


