(defproject scope "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2342"]]

  :profiles { :dev {:dependencies [[devcards "0.1.2-SNAPSHOT"]]
                    :plugins [[lein-cljsbuild "1.0.3"]
                              [lein-figwheel "0.1.4-SNAPSHOT"]] }}

  :source-paths ["src"]

  :cljsbuild {
              :builds [{:id "devcards"
                        :source-paths ["devcards_src" "src"]
                        :compiler {
                                   :output-to "resources/public/devcards/js/compiled/scope_devcards.js"
                                   :output-dir "resources/public/devcards/js/compiled/out"
                                   :optimizations :none
                                   :source-map true}}

                       #_{:id "app"
                          :source-paths ["src"]
                          :compiler {
                                     :output-to "resources/public/js/compiled/scope.js"
                                     :output-dir "resources/public/js/compiled/out"
                                     :optimizations :none
                                     :source-map true}}]}

  :figwheel { :css-dirs ["resources/public/css"] })
