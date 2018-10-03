(ns entomologist.client-server.displayable
  #?(:clj
      (:require [clojure.pprint :refer [pprint]]
                [entomologist.client-server.displayable-protocols
                 :refer [Displayable Displayed]]
                [entomologist.instrument.symbol-info]
                [entomologist.instrument.recur]))
  #?(:clj
      (:import [clojure.lang PersistentArrayMap Fn IPending IDeref]
               [entomologist.instrument.symbol_info VarInfo SpecialFormInfo]
               [entomologist.instrument.recur RecurArguments]))
  #?(:cljs
      (:require [cljs.reader :refer [register-tag-parser!]]
                [cljs.pprint :refer [pprint]]
                [entomologist.client-server.displayable-protocols
                 :refer [Displayable Displayed]]
                [entomologist.client.ui.colors :as colors])))

;; TODO: add an intermediary layer between data representation storage and data
;;       data representation
(defrecord DefaultDisplayed [data]
  Displayed
  (displayed [this] (pr-str (:data this))))

(defrecord NilDisplayed []
  Displayed
  (displayed [this] "nil"))

(defrecord PersistentArrayMapDisplayed [sequence]
  Displayed
  (displayed [this] (pr-str (into (sorted-map) (:sequence this)))))

(defrecord FnDisplayed [fn-str]
  Displayed
  (displayed [this] (:fn-str this)))

(defrecord VarInfoDisplayed [info]
  Displayed
  (displayed
    [this]
    #?(:cljs
        (do [:div.var-info
             [:span.separator "-------------------------"]
             [:br]
             [:span.qualified-name 
              [:span.namespace (-> info :ns str)]
              [:span "/"]
              [:span (str (:name info))]]
             #_[:span.file (-> info :file str)] ;; TODO : keep?
             [:br]
             [:span.arglists (pr-str (:arglists info))]
             [:br]
             [:span.doc {:style {:color (-> @colors/Current-theme
                                            :accent :green)}}
              (if-let [docstring (:doc info)]
                docstring
                "nil")]]))))

(defrecord SpecialFormInfoDisplayed [info]
  Displayed
  (displayed [this] (:doc (:info this))))

(defrecord IPendingDisplayed [status val]
  Displayed
  (displayed [this] [:div.ipending
                     [:div.status
                      [:span#title ":status"]
                      [:span#value (str status)]]
                     [:div.val
                      [:span#title ":val"]
                      [:span#value (with-out-str (pprint val))]]]))

;; TODO : what happens when we have custom records/types nested inside seqs?
(defrecord RecurArgumentsDisplayed [args]
  Displayed
  (displayed [this] (->> args
                         (map #(with-out-str (pprint %)))
                         (interpose " ")
                         (apply str)
                         (str "recur : "))))

#?(:cljs
    (do (register-tag-parser!
          "entomologist.client_server.displayable.DefaultDisplayed"
          map->DefaultDisplayed)
        (register-tag-parser!
          "entomologist.client_server.displayable.NilDisplayed"
          map->NilDisplayed)
        (register-tag-parser!
          "entomologist.client_server.displayable.PersistentArrayMapDisplayed"
          map->PersistentArrayMapDisplayed)
        (register-tag-parser!
          "entomologist.client_server.displayable.FnDisplayed"
          map->FnDisplayed)
        (register-tag-parser!
          "entomologist.client_server.displayable.VarInfoDisplayed"
          map->VarInfoDisplayed)
        (register-tag-parser!
          "entomologist.client_server.displayable.SpecialFormInfoDisplayed"
          map->SpecialFormInfoDisplayed)
        (register-tag-parser!
          "entomologist.client_server.displayable.IPendingDisplayed"
          map->IPendingDisplayed)
        (register-tag-parser!
          "entomologist.client_server.displayable.RecurArgumentsDisplayed"
          map->RecurArgumentsDisplayed)))


#?(:clj
    (do       
      ;; TODO : implement data size limit heuristics
      (extend-protocol Displayable
        Object              (display [this] (DefaultDisplayed. this))
        nil                 (display [this] (NilDisplayed.))
        PersistentArrayMap  (display [this] (PersistentArrayMapDisplayed.
                                              (seq this)))
        Fn                  (display [this] (FnDisplayed. (str this)))
        VarInfo             (display [this] (VarInfoDisplayed.
                                              (-> (into {} this)
                                                  (update-in [:ns] str))))
        SpecialFormInfo     (display [this] (SpecialFormInfoDisplayed.
                                              (into {} this)))
        IPending            (display [this]
                                     (let [r? (realized? this)
                                           status (if r? :ready :pending)]
                                       (IPendingDisplayed.
                                         status
                                         (when r?
                                           (if (instance? IDeref this)
                                             @this
                                             this)))))
        RecurArguments      (display [this] (RecurArgumentsDisplayed.
                                              (.args this))))))



