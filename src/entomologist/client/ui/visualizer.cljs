(ns entomologist.client.ui.visualizer
  (:use [cljs.pprint :only [pprint]])
  (:require [entomologist.client-server.displayable
             :refer [VarInfoDisplayed]]
            [entomologist.client-server.displayable-protocols
             :refer [displayed]]
            [entomologist.client.store :as store]
            [entomologist.client.ui.table :refer [table]]
            [entomologist.client.ui.highlight
             :refer [get-all-highlights]]))

(defn context-table [ctx-vs cst-node]
  (let [xxx (map (fn [[ctx v]]
                   (let [fn-ctxs (filter (comp #{:scope} :key)
                                         ctx)]
                     (concat
                       [["#" (-> fn-ctxs last :time-mark)]]
                       (mapcat
                         (fn [{:keys [value]}]
                           (map (juxt key (comp displayed :value val))
                                (sort-by (comp :path val)
                                         value)))
                         fn-ctxs)
                       [["value" (displayed v)]])))
                 ctx-vs)
        headers (map first (first xxx))
        data (->> xxx
                  (map #(map second %))
                  (sort-by first)
                  (map-indexed #(cons (inc %1) (rest %2))))
        vs (distinct (map last data))]
    (if (= 1 (count vs))
      (if (= 1 (count data))
        (first vs)
        [:div.always "Always" [:br] (first vs)])
      [:div.visualizer-table
       [table headers data]])))

(defn visualizer [reference] ;; = [ns insect-name path] or nil
  (let [ctx-vs (when reference
                 (apply store/get-contextualized-values reference))]
    [:div.visualizer
     (case (count ctx-vs)
       0 "-"
       1 (-> ctx-vs first val displayed)
       (if-let [info (some->> ctx-vs
                          (filter (fn [[ctx v]]
                                    (instance? VarInfoDisplayed v)))
                          first val)]
         [:div
          (displayed info)
          [context-table
           (remove (fn [[ctx v]]
                     (instance? VarInfoDisplayed v))
                   ctx-vs)
           (apply store/get-cst-node reference)]]
         [context-table ctx-vs (apply store/get-cst-node reference)]))]))



