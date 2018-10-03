(ns entomologist.explorations.rewrite-clj
  (:use clojure.pprint)
  (:require [rewrite-clj.node :as n]
            [entomologist.self.read :refer (raw-code)
             :reload true]))

(def toto :toto)

(let [raw (-> (raw-code
                :a
                ;; comment
                :b)
              :children
              (nth 3))]
  (println "----------- Comment -----------")
  (println raw)
  (pprint raw)
  (println "-------------------------------")
  (newline))

  
  (let [raw (raw-code :a :b :c)]
    (println "--------- forms-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (raw-code 12)]
    (println "--------- int-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (raw-code :a)]
    (println "--------- keyword-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (raw-code #^{:tag "some-tag"} toto)]
    (println "--------- meta-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (raw-code '(toto))]
    (println "--------- quote-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (raw-code #'toto)]
    (println "--------- reader-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (raw-code #inst "2016-01-23T20:59:18.249-00:00")]
    (println "----- reader-macro-node -------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (raw-code @toto)]
    (println "--------- deref-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (raw-code "a string
                      with newline")]
    (println "--------- string-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (-> (raw-code :a #_(toto) :b)
                :children
                (nth 2))]
    (println "--------- uneval-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (-> (raw-code
                  :a
                  :b)
                :children
                (nth 1))]
    (println "--------- newline-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (raw-code [:a :b :c])]
    (println "--------- seq-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (raw-code toto)]
    (println "--------- token-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))

  
  (let [raw (-> (raw-code :a :b)
                :children second)]
    (println "--------- whitespace-node ---------")
    (println raw)
    (pprint raw)
    (println "-------------------------------")
    (newline))
  
