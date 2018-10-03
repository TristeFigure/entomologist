(ns entomologist.rewrite-clj.complements
  (:require [rewrite-clj.parser :as p]
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]
            [rewrite-clj.zip.base :refer [edn*]])
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

;; TODO make sure any of these nodes can be understood by -o
(defn rewrite-clj-node? [n]
  (boolean
    (#{CommentNode FormsNode IntNode KeywordNode MetaNode QuoteNode
       ReaderNode ReaderMacroNode DerefNode StringNode UnevalNode
       NewlineNode SeqNode TokenNode WhitespaceNode}
       (class n))))

(defn not-code-node? [n]
  (boolean
    (#{CommentNode NewlineNode WhitespaceNode UnevalNode}
       (class n))))

(defn seq-node? [n]
  (instance? SeqNode n))

(defn quote-node? [n]
  (instance? QuoteNode n))

(defn uneval-node? [n]
  (instance? UnevalNode n))

(defn newline-node? [n]
  (instance? NewlineNode n))

(defn token-node? [n]
  (instance? TokenNode n))

(defn keyword-node? [n]
  (instance? KeywordNode n))

(defn string-node? [n]
  (instance? StringNode n))

(defn comment-node? [n]
  (instance? CommentNode n))

(defn list-node? [n]
  (and (seq-node? n)
       (= (:tag n) :list)))

(defn vector-node? [n]
  (and (seq-node? n)
       (= (:tag n) :vector)))

(defn map-node? [n]
  (and (seq-node? n)
       (= (:tag n) :map)))

(defn set-node? [n]
  (and (seq-node? n)
       (= (:tag n) :set)))

(defn def-form-node? [n]
  (boolean
    (and (list-node? n)
         (when-let [x (-> n :children first)]
           (and (token-node? x)
                (->> x :value name (re-find #"^def")))))))

(defn map-code [f nodes]
  (map (fn [c]
         (if (not-code-node? c)
           c
           (f c)))
       nodes))


(defn rights [loc]
  (let [rs (->> loc
                clojure.zip/rights
                (drop-while not-code-node?))]
    (if (> (count rs) 1)
      (n/forms-node rs)
      (first rs))))

(defn remove-rights [loc]
  (let [pth (clojure.zip/path loc)
        up-to-loc (partial z/skip z/up #(not= (clojure.zip/path %) pth))]
    (loop [xloc loc
           acc []]
      (if-let [right-loc (z/right xloc)]
        (let [new-xloc (-> right-loc z/remove up-to-loc)]
          (recur new-xloc (conj acc (z/node right-loc))))
        [xloc acc]))))

(defn to-map
  "Convert a rewrite-clj node into a map with a :class key"
  [node]
  (assoc (into {} node)
    :class (cond
             (instance? CommentNode node) :comment-node
             (instance? FormsNode node) :forms-node
             (instance? IntNode node) :int-node
             (instance? KeywordNode node) :keyword-node
             (instance? MetaNode node) :meta-node
             (instance? QuoteNode node) :quote-node
             (instance? ReaderNode node) :reader-node
             (instance? ReaderMacroNode node) :reader-macro-node
             (instance? DerefNode node) :deref-node
             (instance? StringNode node) :string-node
             (instance? UnevalNode node) :uneval-node
             (instance? NewlineNode node) :newline-node
             (instance? SeqNode node) :seq-node
             (instance? TokenNode node) :token-node
             (instance? WhitespaceNode node) :whitespace-node
             :else (throw (ex-info (str "to-map error > unsupported class : " (class node))
                                   {:type :to-map-unsupported-class
                                    :class (class node)
                                    :node node})))))

(defn template* [code-node replacements-nodes]
  (reduce (fn [n [s v]]
            (let [s (cond
                      (symbol? s)  s
                      (string? s)  (symbol s)
                      (keyword? s) (->> s str
                                        rest (apply str) ;; removes initial ":"
                                        symbol))]
              (loop [z (edn* n)]
                (if-let [loc (z/find-next-value z z/next s)]
                  (recur (z/replace loc v))
                  (z/root z)))))
          code-node
          replacements-nodes))

;; TODO : really useful ?
(defn reindent [n offset]
  ;; first line is ok,
  ;; but the next ones are off to the right by 'offset'
  (let [s (-> n n/string (clojure.string/split #"\n"))]
    (-> (str (first s) \newline
             (->> s rest
                  (map #(->> % (drop offset) (apply str)))
                  (interpose "\n")
                  (apply str)))
        p/parse-string)))

(defn template
  ([code]
   (template code {}))
  ([code replacements]
   (let [adapt (fn [x]
                 (if (rewrite-clj-node? x)
                   x
                   (p/parse-string
                     (if (string? x)
                       x
                       (with-out-str
                         (clojure.pprint/write
                           x :dispatch clojure.pprint/code-dispatch))))))
         code (adapt code)
         replacements (->> replacements
                           (map (fn [[k v]]
                                  [k (adapt v)]))
                           (into {}))]
     (template* code replacements))))