(ns entomologist.client.ui.main
  (:use [cljs.pprint :only [pprint]])
  (:require [reagent.core :as reagent]
            [re-com.core
             :refer [horizontal-tabs vertical-pill-tabs h-box
                     md-icon-button]]
            [entomologist.client.store :as store]
            [entomologist.client.ui.cst :refer [cst-component]]
            [entomologist.client.ui.visualizer :refer [visualizer]]
            [entomologist.client.ui.focus :refer [get-focus]]
            [entomologist.client.ui.dev :as dev]
            [entomologist.client.ui.colors :as colors]
            [entomologist.client.ui.utils :refer [should-update?]]))


(declare cst-list)

(defn test-main []
  [:div.main {:style {:background-color (-> @colors/current-theme
                                                   :background)
                           :color (-> @colors/current-theme
                                      :content-1)}}
   [cst-list (store/get-insects-list)]
   [visualizer (get-focus)]
   
   [dev/display-guard "(get-focus)"
    (fn []
      [:pre (with-out-str (pprint (get-focus)))])]
   [dev/display-guard "visualized's values"
    (fn []
      [:pre (with-out-str (pprint (apply store/get-contextualized-values
                                         (get-focus))))])]
   [dev/display-guard "@store/state"
    (fn []
      [:pre (with-out-str (pprint @store/state))])]])

(defprotocol Tabbable
  (label [this])
  (tab-type [this])
  (removable? [this]))

(defrecord NamespacesTab [id]
  Tabbable
  (label [this] "Namespaces")
  (tab-type [this] :namespaces)
  (removable? [this] false))

;; TODO : remove
(declare remove-tab! selected-tab-id tabs)
(defrecord Rien [id]
  Tabbable
  (label [this] [:span "Rien"
                 @selected-tab-id
                 ; (when (= id @selected-tab-id)
                 ;   [md-icon-button
                 ;    :md-icon-name "zmdi-close"
                 ;    :size         :smaller
                 ;    :style        {:display "inline"
                 ;                   :margin-left "5px"
                 ;                   :vertical-align "middle"}
                 ;    :on-click (fn [_e]
                 ;                ;; select previous tab
                 ;                (when (= id @selected-tab-id)
                 ;                  (reset! selected-tab-id
                 ;                          (->> @tabs
                 ;                               reverse
                 ;                               (drop-while #(not= % this))
                 ;                               second .-id)))
                 ;                (println "tabs before :" @tabs)
                 ;                (remove-tab! this)
                 ;                (println "tabs :" @tabs)
                 ;                (println "selected-tab-id :" @selected-tab-id))])
                 ])
  (tab-type [this] :rien)
  (removable? [this] true))

(def tabs
  (reagent/atom []))

(def selected-tab-id
  (reagent/atom nil))

(defn add-tab! [^Tabbable tab]
  (swap! tabs conj tab))

(defn remove-tab! [^Tabbable tab]
  (when-not (removable? tab)
    (throw (ex-info "Can't remove unremovable tab"
                    {:type :tried-removing-unremovable-tab
                     :tab tab})))
  (let [tab-id (.-id tab)]
    (swap! tabs (fn [x]
                  (doall (vec (remove #(= (.-id %) tab-id)
                                      x)))))))

(defn init-tabs []
  (let [t (new NamespacesTab (count @tabs))]
    (add-tab! t)
    (add-tab! (new Rien (count @tabs)))
    (reset! selected-tab-id (.-id t))))

(init-tabs)



(declare tab-content)

(def main
  (with-meta
    (fn []
      [:div.main {:style {:color (:content-1 @colors/current-theme)}}
       [horizontal-tabs
        :tabs tabs
        :model selected-tab-id
        :label-fn label
        :on-change #(reset! selected-tab-id %)]
       [tab-content (first (filter #(= (.-id %) @selected-tab-id)
                                   @tabs))]
       [visualizer (get-focus)]])
    {:should-component-update (fn [this old-argv new-argv]
                                @should-update?)}))


(defmulti tab-content tab-type)

(def selected-namespace
  (reagent/atom (first (store/get-namespaces))))

(defn get-selected-namespace []
  @selected-namespace)

(defn set-selected-namespace! [id]
  (reset! selected-namespace id))

(defmethod tab-content :namespaces [_]
  (let [sel-ns (get-selected-namespace)
        debugs (filter #(= sel-ns (first %))
                       (store/get-insects-list))]
    [:div "toto"]
    [:input {:type "button"
             :on-click (fn [_e] (remove-tab! (second @tabs)))}]
    [h-box
     :gap "4px"
     :children [[vertical-pill-tabs
                 :tabs (map (fn [n] {:id n})
                            (store/get-namespaces))
                 :model (get-selected-namespace)
                 :on-change set-selected-namespace!
                 :id-fn :id
                 :label-fn :id]
                [cst-list debugs]]]))

(defmethod tab-content :rien [_]
  [:div "rien"])

(defn cst-list [debugs]
  [:ul
   (for [[ns name cst] debugs]
     ^{:key [ns name]} [:li [cst-component ns name cst]])])




