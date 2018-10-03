(ns entomologist.server.projection
  (:use clojure.pprint)
  (:require [entomologist.rewrite-clj.complements
             :refer [to-map rewrite-clj-node?]]
            [entomologist.instrument.symbol-info]
            [entomologist.client-server.displayable-protocols
             :refer [display]]
            [entomologist.client-server.displayable
             :reload true]))

(def client-attributes-plan
  {:db/id               identity
   :insect/name         str
   :insect/namespace    str
   :insect/type         identity
   :insect/cst          #(->> % 
                              (clojure.walk/prewalk
                                (fn [x]
                                  (if (rewrite-clj-node? x)
                                    (-> (to-map x)
                                        (dissoc
                                          :seq-fn :format-string
                                          :wrap-length))
                                    x))))
   :insect/_observation identity ;; TODO : keep ?
   :observation/context (fn [ctx]
                          (map
                            (fn [x]
                              (if (= (:key x) :scope)
                                (assoc x :value
                                  (->> (:value x)
                                       (map (fn [[k v]]
                                              [k (assoc v :value
                                                   (display (:value v)))]))
                                       (into {})))
                                x))
                               ctx))
   :observation/path    identity
   ;; Dirty hack around the impossibility to store nil
   ;; in datascript attributes.
   ;; See entomologist.client-server.displayable
   :observation/value   #(display (if (= % :entomologist.store/nil) nil %))})

(let [ks (-> client-attributes-plan keys set)]
  (defn adapt-for-client [m]
    (->> (select-keys m ks)
         (map (fn [[k v]]
                [k ( (client-attributes-plan k)  v )]))
         (into {}))))

(defn most-recent-data [insect-m]
  (let [obs (:insect/observation insect-m)]
    (if-not (empty? obs)
      (let [max-exec-time (->> obs
                               (apply max-key
                                      #(let [v (-> % :observation/context
                                                   first :value)]
                                         (+ (.getTime (first v))
                                            (second v))))
                               :observation/context first :value)
            filtered-obs (filter #(= (-> % :observation/context first :value)
                                     max-exec-time)
                                 obs)]
        (assoc insect-m :insect/observation filtered-obs))
      insect-m)))

(defn access-by-path-then-context [insect-m]
  (let [obs (:insect/observation insect-m)]
    (assoc insect-m
      :insect/observation
      (->> obs
           (group-by :observation/path)
           (map (fn [[pth obss]]
                  [pth (->>
                         obss
                         (group-by :observation/context)
                         (map (fn [[ctx obs]]
                                [ctx (-> obs first :observation/value)]))
                         (into {}))]))
           (into {})))))

(defn project-insect [insect-m]
  (->> (assoc (adapt-for-client insect-m)
         :insect/observation
         (map adapt-for-client
              (:insect/observation insect-m)))
       most-recent-data
       access-by-path-then-context))


