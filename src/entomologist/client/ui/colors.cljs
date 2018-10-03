(ns entomologist.client.ui.colors
  (:require [reagent.core :as reagent]))

(def colors
  (cycle ["yellow" "orange" "red" "magenta" "violet" "blue" "cyan" "green"])) 

(def solarized
  {:base03  "#002b36"
   :base02  "#073642"
   
   :base01  "#586e75"
   :base00  "#657b83"
   :base0   "#839496"
   :base1   "#93a1a1"
   
   :base2   "#eee8d5"
   :base3   "#fdf6e3"
   
   :yellow  "#b58900"
   :orange  "#cb4b16"
   :red     "#dc322f"
   :magenta "#d33682"
   :violet  "#6c71c4"
   :blue    "#268bd2"
   :cyan    "#2aa198"
   :green   "#859900"})

(def dark-colors
  {:background  (:base03 solarized)
   :background-highlight (:base02 solarized)
   :content-0   (:base01 solarized)
   :content-1   (:base0 solarized)
   :content-2   (:base1 solarized)
   :content-3   (:base2  solarized)
   :content-4   (:base3  solarized)})

(def light-colors
  {:background  (:base3 solarized)
   :background-highlight (:base2 solarized)
   :content-0   (:base1 solarized)
   :content-1   (:base00 solarized)
   :content-2   (:base01 solarized)
   :content-3   (:base02  solarized)
   :content-4   (:base03  solarized)})

(def accent-colors
  (select-keys solarized
               [:yellow :orange :red :magenta :violet :blue :cyan :green]))

(def obscene-colors
  {:yellow  "gold"
   :orange  "orange"
   :red     "red"
   :magenta "magenta"
   :violet  "violet"
   :blue    "blue"
   :cyan    "cyan"
   :green   "green"})

(def solarized-dark
  (assoc dark-colors
    :accent  accent-colors
    :obscene obscene-colors))

(def solarized-light
  (assoc light-colors
    :accent  accent-colors
    :obscene obscene-colors))


(defn color-cycle [color-map]
  (cycle
    (-> color-map
        ((juxt :yellow :orange :red :magenta :violet :blue :cyan :green)))))

(def current-theme
  (reagent/atom solarized-dark))




