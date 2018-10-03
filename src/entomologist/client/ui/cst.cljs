(ns entomologist.client.ui.cst
  (:use [cljs.pprint :only [pprint]])
  (:require [reagent.core :as reagent]
            [entomologist.client.store :as store]
            [entomologist.client.ui.colors :as colors]
            [entomologist.client.ui.focus
             :refer [set-hard-focus! set-soft-focus! get-hard-focus
                     get-focus unset-soft-focus! unset-hard-focus!]]))

(declare node-component
         render-node
         node-mouse-hover
         node-mouse-out
         node-mouse-clicked)

; ~|~ |_  _     _   _  _  _  ,_ |- .    |
;  |  || (/_   (/_ _\ _\ (/_ || |_ | (| |
;
(defn scope-var-node-paths [reference] ;; = [ns insect-name path]
  (->> reference
       (apply store/get-contextualized-values)
       keys
       first
       (filter #(-> % :key (= :scope)))
       (mapcat #(->> % :value
                  (map (fn [[k v]]
                         (:path v)))))))

(defn cst-component [ns insect-name cst]
  (fn [ns insect-name cst]
    (let [[f-ns f-insect-name _pth :as foc] (get-focus)
          highlights (when (= [ns insect-name] [f-ns f-insect-name])
                       (->> (scope-var-node-paths foc)
                            (map-indexed (fn [i x] [x i]))
                            (into {})))]
      [:div.cst {:style {:color (:content-1 @colors/current-theme)}}
       [node-component ns insect-name cst
        {:highlights highlights}]])))

(defn node-component [ns insect-name node {:keys [highlights] :as opts}]
  (let [mini-state (reagent/atom {:hover false})]
    (fn [ns insect-name node {:keys [highlights] :as opts}]
      (let [pth (:path node)
            focus-ref (get-hard-focus)
            highlight-color (when highlights
                              (when-let [idx (get highlights pth)]
                                (nth (colors/color-cycle
                                       (:obscene @colors/current-theme))
                                     idx)))]
        [:div {:class "wrapper"
               :style {:color (or highlight-color
                                  (when (:hover @mini-state)
                                    (:content-3 @colors/current-theme))
                                  (:content-1 @colors/current-theme))
                       :background-color (if (= focus-ref [ns insect-name pth])
                                           (-> @colors/current-theme
                                               :obscene :yellow)
                                           (when (:hover @mini-state)
                                             (-> @colors/current-theme
                                                 :background-highlight)))
                       :font-weight (when highlight-color "bold")}
               :on-mouse-over
               (partial node-mouse-hover ns insect-name node mini-state)
               :on-mouse-out
               (partial node-mouse-out mini-state)
               :on-click
               (partial node-mouse-clicked ns insect-name node)}
         (render-node ns insect-name node opts)]))))


; |)  _  ,_  |  _
; |\ (/_ || (| (/_ |`
;
(defmulti render-node #(-> %& (nth 2) :class)) ;; dispatch on node class

(defmethod render-node :default [ns insect-name node opts]
  [:div "No implementation for node with class " (str (:class node))
   [:div (str node)]])

(defmethod render-node :forms-node [ns insect-name n opts]
  [:span {:class "node formsnode"}
   (map-indexed (fn [i c]
                  ^{:key i} [node-component ns insect-name c opts])
                (:children n))])

(def ^:private delimiters
  {:list   ["(" ")"]
   :vector ["[" "]"]
   :map    ["{" "}"]})

(defmethod render-node :seq-node [ns insect-name n opts]
  (let [[LLL RRR] (delimiters (:tag n))]
    [:div {:class (str "node seqnode " (name (:tag n)))}
     LLL
     (map-indexed
       (fn [i c]
         ^{:key i} [node-component ns insect-name c opts])
       (:children n))
     RRR]))

(defmethod render-node :token-node [ns insect-name n opts]
  [:div {:class "node token"
         :style {:color (let [x (-> @colors/current-theme :accent)
                              s (:string-value n)
                              v (:value n)]
                          (get x (cond
                                   (#{"def" "defn"} s)
                                   :red
                                   
                                   (#{"let" "fn" "for" "doseq" "if-let"
                                      "when-let"}   s)
                                   :blue
                                   
                                   (#{"map" "reduce" "filter" "remove" "take"
                                      "drop" "take-while" "drop-while"} s)
                                   :cyan
                                   
                                   (number? v)
                                   :orange
                                   
                                   (keyword? v)
                                   :yellow
                                   
                                   (string? v)
                                   :green)))}}
   (:string-value n)])

(defmethod render-node :string-node [ns insect-name n opts]
  [:div {:class "node string"
         :style {:color (-> @colors/current-theme :accent :green)}}
   (let [explicit-string (fn [x]
                           (str "\"" x "\""))]
     (if (= 1 (count (:lines n)))
        (-> n :lines first explicit-string)
        (->> (:lines n)
             (map-indexed (fn [i x]
                            [(explicit-string x)
                             [:br {:key (inc i)}]]))
             (apply concat))))])

(defmethod render-node :keyword-node [ns insect-name n opts]
  [:div {:class "node keyword"}
   (str (:k n))])

(defmethod render-node :whitespace-node [ns insect-name n opts]
  [:div {:class "node whitespace"}
   (-> (:whitespace n)
       (clojure.string/replace "\t" "  ")
       count
       (repeat "_")
       (->> (apply str)))])

(defmethod render-node :newline-node [ns insect-name n opts]
  [:div {:class "node newline"}
   (->> (:newlines n)
        count
        range
        (map (fn [x]
               [:br {:key x}])))])

(defmethod render-node :comment-node [ns insect-name n opts]
  [:div {:class "node coment"
         :style {:color (@colors/current-theme :content-0)}}
   (str \; (:s n))
   [:br]])


; |\/|        _  _    . ,_ |-  _         _ |- .    ,_
; |  | () L| _\ (/_   | || |_ (/_ |` (| (_ |_ | () ||
;
(defn subpath?
  "Tells whether pth-a is a subpath of pth-b"
  [pth-a pth-b]
  (= pth-a (take (count pth-a) pth-b)))

(defn node-mouse-hover [ns insect-name node mini-state e]
  (let [pth (:path node)
        last-hover (store/get-last-hovered-path)
        perform (fn []
                  (store/set-last-hovered-path! pth)
                  ;; whitespace and newlines node do not have a path
                  (when-not (nil? pth)
                    (swap! mini-state assoc :hover true)
                    (set-soft-focus! [ns insect-name pth])))]
    (if last-hover   
      (when (and pth (not (subpath? pth last-hover)))
        (perform))
      (perform))))

(defn node-mouse-out [mini-state e]
  (store/set-last-hovered-path! nil)
  (swap! mini-state assoc :hover false)
  (unset-soft-focus!))

(defn node-mouse-clicked [ns insect-name node e]
  (let [pth (:path node)
        last-hover (store/get-last-hovered-path)
        hard-foc (get-hard-focus)]
    (when (= pth last-hover)
      (if (= hard-foc [ns insect-name pth])
        (unset-hard-focus!)
        (set-hard-focus! [ns insect-name pth])))))
