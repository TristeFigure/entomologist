(defproject entomologist "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238"]
                 [org.clojure/core.async "0.4.474"]

                 ;; TODO : remove ?
                 [org.clojure/core.match "0.3.0-alpha5"]

                 [rewrite-clj "0.4.12"]
                 [cljfmt "0.3.0"] ;; TODO : really useful ?
                 [org.clojure/tools.namespace "0.2.11"]

                 [com.rpl/specter "0.13.1"]

                 [http-kit "2.1.18"]
                 [ring "1.4.0"]
                 [ring/ring-json "0.4.0"]
                 [compojure "1.4.0"]
                 [hiccup "1.0.5"]
                 [com.taoensso/sente "1.12.0"]
                 [reagent "0.8.1"]
                 [re-com "0.8.0"]
                 [datascript "0.13.3"] ;; TODO : remove
                 ]

  :plugins [[lein-cljsbuild "1.1.2"]
            [lein-figwheel "0.5.0-2"]
            ; [lein-autoreload "0.1.0"]
            ]
  :clean-targets ^{:protect false} ["resources/public/js"]
  :cljsbuild {:builds
              {:dev {:source-paths ["src/entomologist/client"
                                    "src/entomologist/client_server"]
                     :figwheel {:on-jsload entomologist.client.core/fig-reload}
                     :compiler {:main entomologist.client.core
                                :asset-path "js"
                                :output-to "resources/public/js/main.js"
                                :output-dir "resources/public/js"
                                :optimizations :none
                                :source-map true}}}}
  ;; to scrap clojuredocs.org examples
  :profiles {:dev {:dependencies [[enlive "1.1.6"]]}})
