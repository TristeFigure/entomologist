(ns entomologist.client.ui.table
  (:use [cljs.pprint :only [pprint]])
  (:require [reagent.core :as reagent]
            [entomologist.client.ui.colors :as colors]))

(defn td-mouse-over [mini-state i j _e]
  (swap! mini-state assoc :hovered [i j]))

(defn td-mouse-out [mini-state _e]
  (swap! mini-state dissoc :hovered))

(defn adjacent [index pred coll]
  (let [value (nth coll index)
        coll (map-indexed (fn [i x] [i x])
                          coll)
        rev-index (- (count coll) index)
        found (concat (->> (reverse coll)
                           (drop rev-index)
                           (take-while (fn [[i x]] (pred value x)))
                           reverse
                           (map first))
                      (->> coll
                           (drop index)
                           (take-while (fn [[i x]] (pred value x)))
                           (map first)))]
    found))

(defn table [headers data]
  (let [mini-state (reagent/atom {})]
    (fn [headers data]
      (let [[hi hj] (if-let [h (:hovered @mini-state)]
                      h [nil nil])
            adjacents (when hi
                        (mapv (fn [j]
                                (set (adjacent hi #(= (nth %1 j) (nth %2 j))
                                               data)))
                              (range (count (first data)))))
            similar? (if hi
                       (fn [i j]
                         (let [adj? (get adjacents j)]
                           (and adj? (adj? i))))
                       (constantly false))]
        [:table {:style {:color (-> @colors/current-theme :content-1)}}
         [:thead
          [:tr {:style {:color (-> @colors/current-theme :content-0)}}
           (doall (map-indexed
                    (fn [j h]
                      ^{:key j} [:th {:style {:color (when (= j hj)
                                                       (-> @colors/current-theme
                                                           :content-3))}}
                                 h])
                    headers))]]
         [:tbody
          (doall
            (map-indexed
              (fn [i d]
                [:tr {:key i
                      :style
                      {:background-color
                       (get @colors/current-theme
                            (cond
                              (-> @mini-state :hovered first (= i)) :content-3
                              (zero? (rem i 2)) :background
                              :else             :background-highlight))}}
                 (doall
                   (map-indexed
                     (fn [j x]
                       (with-redefs [colors/current-theme
                                     (if (= i hi)
                                       (atom (if (= @colors/current-theme
                                                    colors/solarized-light)
                                               colors/solarized-dark
                                               colors/solarized-light))
                                       colors/current-theme)]
                         ^{:key j} [:td {:style
                                         {:color (when (and (= j hj)
                                                            (similar? i j))
                                                   (-> @colors/current-theme
                                                       :content-3))}
                                         :on-mouse-over
                                         (partial td-mouse-over mini-state i j)
                                         :on-mouse-out
                                         (partial td-mouse-out mini-state i j)}
                                    x]))
                     d))])
                data))]]))))

