(ns entomologist.gentests
  (:use clojure.pprint)
  (:require [org.httpkit.client :as http]
            [net.cgrand.enlive-html :as e]))

(def clojure-core-index-url
  "http://clojuredocs.org/clojure.core")

(defn ensure-folders [str-name]
  (let [f (clojure.java.io/file str-name)]
    (when-not (.exists f)
      (.mkdirs f))
    f))

(defn generate-tests []
  (let [index-src (:body @(http/get clojure-core-index-url))
        index-nodes (e/html-resource (java.io.StringReader. index-src))
        names (->> (e/select index-nodes [:dt :> :a])
                   (map (fn [x]
                          (->> x :attrs :href
                               ((juxt #(->> % (drop 14) (apply str))
                                      #(str "http://clojuredocs.org" %))))))
                   (into {}))
        f (ensure-folders (str "test/entomologist/generated/clojure/core"))]
    (doseq [[name url] [(first names)]]
      (let [src (:body @(http/get url))
            nodes (e/html-resource (java.io.StringReader. src))
            data (-> (->> (e/select nodes [:script])
                          first :content first
                          (drop 31)
                          reverse
                          (drop 8)
                          reverse
                          (apply str))
                     (clojure.string/replace "\\\"" "\"")
                     read-string
                     :examples
                     (->> (map
                            #(-> % :body
                                 (clojure.string/replace #"user=>\s*" "")
                                 (clojure.string/replace "\\n" "\n")))))
            adapted-name (clojure.string/replace name "-" "_")
            filename (str adapted-name ".clj")
            ff (clojure.java.io/file f filename)]
        (spit ff (str "(ns entomologist.generated.clojure.core."
                      adapted-name ")\n\n"))
        (doseq [d data]
          (spit ff d :append true)
          (when-not (= d (last data))
            (spit ff "\n\n;; -------------\n\n" :append true)))))))
(generate-tests)
