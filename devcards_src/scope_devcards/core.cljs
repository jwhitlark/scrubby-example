(ns scope-devcards.core
  (:require
   [devcards.core :as dc :include-macros true]
   [scope.core :as scope]
   [clojure.string :as str]
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [sablono.core :as sab :include-macros true]
   [goog.events :as events]
   [cljs.core.async :as async :refer [>! <! put! chan]])
  (:require-macros
   [devcards.core :refer [defcard is are= are-not=]]
   [cljs.core.async.macros :refer [go go-loop]])
  (:import [goog.events EventType]))

(enable-console-print!)

(devcards.core/start-devcard-ui!)
(devcards.core/start-figwheel-reloader!)

;; remember to run lein figwheel and then browse to
;; http://localhost:3449/devcards/index.html

;; -------------------- Constants --------------------

(def buffer-max 100)
(def graph-width 100)
(def graph-height 30)

;; -------------------- Helpers --------------------

(defn log [x]
  (.log js/console x))

(defn by-id [id]
  (.getElementById js/document id))

(defn set-states!
  "Given a map, for each k/v pair, set! that key in the state to the
  given value."
  [owner desired]
  (doall (map #(om/set-state! owner (key %) (val %)) desired)))

(def sin #(.sin js/Math %))

(defn move-absolute [x y]
  {:pre [(and (not (nil? x))
              (not (nil? y)))]}
  (str "M" x "," y))

(defn line-absolute [x y]
  {:pre [(and (not (nil? x))
              (not (nil? y)))]}
  (str "L" x "," y))

(defn line-relative [x y]
  {:pre [(and (not (nil? x))
              (not (nil? y)))]}
  (str "l" x "," y))

(def close-shape (constantly "Z"))

(defn box [w h]
  (->> [(move-absolute 0 0)
        (line-absolute graph-width 0)
        (line-absolute graph-width graph-height)
        (line-absolute 0 graph-height)
        (close-shape)]
   (str/join " ")))

(defn calc-start
  "Figure out the correct place to start drawing our graph."
  [ampl]
  (move-absolute 0.1 (-> graph-height (/ 2) (- ampl))))

(defn sin-seq
  "Given a frequency, generate a sequence to use as the :d value of an
  svg path element."
  [freq ampl]
  ;; FIXME: Need to scale these so that low values don't cause jaggies  (jw 14-09-27)
  (->> (for [x (range (* 2 freq))]
         (line-relative (/ 50 freq)
                        (* ampl (sin x))))
       (cons (calc-start ampl))
       (str/join " ")))

(defn graph
  "Draw a little sparkline sine-wave graph."
  [freq ampl]
  [:div [:svg
         {:style {:width (str graph-width "px")
                  :height (str graph-height "px")}}
         ;; the frame around the graph
         [:path {:cs "100,100"
                 :d (box graph-width graph-height)
                 :stroke "blue" :stroke-width "1"
                 :fill "none"}]
         ;; graph itself
         [:path {:cs "100,100"
                 :d (sin-seq freq ampl)
                 :stroke "green" :stroke-width "1"
                 :fill "none"}]]])

(defn simplify-event
  [evt]
  {:x (.-clientX evt)
   :y (.-clientY evt)
   :type (.-type evt)
   :e evt})

;; handler
(defn handle-scrubber-evt [cursor owner evt-type evt]
  (case evt-type
    :down (set-states! owner {:start-y (.-offsetY evt)
                              :start-x (.-offsetX evt)
                              :capturing true})
    :up (set-states! owner {:start-y nil
                            :start-x nil
                            :capturing false})
    :move (if (om/get-state owner :capturing)
            (let [x-diff (- (.-offsetX evt) (om/get-state owner :start-x))
                  y-diff (- (.-offsetY evt) (om/get-state owner :start-y))]
              (om/transact! cursor :x (partial + x-diff))
              (om/transact! cursor :y #(- % y-diff))
              (set-states! owner {:start-x (.-offsetX evt)
                                  :start-y (.-offsetY evt)})))))

;; scrubber mechanics
(defn scrubbable [cursor owner {:keys [build-fn id]}]
  (reify
    om/IInitState
    (init-state [_]
      {:mouse-chan (chan (async/dropping-buffer buffer-max))
       :capturing false
       :start-x nil
       :start-y nil})
    om/IWillMount
    (will-mount [_]
      (let [mouse-chan (om/get-state owner :mouse-chan)
            handler (partial handle-scrubber-evt cursor owner)]
        (go-loop []
          (apply handler (<! mouse-chan))
          (recur))))
    om/IDidMount
    (did-mount [_]
      (let [node (by-id id)
            mouse-chan (om/get-state owner :mouse-chan)]
        (events/listen node "mousemove" #(put! mouse-chan [:move %]))
        (events/listen node "mousedown" #(put! mouse-chan [:down %]))
        (events/listen node "mouseup" #(put! mouse-chan [:up %]))))
    om/IRenderState
    (render-state [_ state]
      (sab/html [:div {:id id
                       :class  "scrubber"
                       :style {:-webkit-user-select "none"}}
                 build-fn]))))

;; Make something scrubbable
(defn scrubbable-widget [id target cursor owner]
     (reify
       om/IRenderState
       (render-state [_ state]
         (sab/html
          [:div
           (om/build scrubbable
                     (:scrubbable cursor)
                     {:opts {:id id
                             :build-fn (om/build target (:scrubbable cursor))}})]))))

;; View components
(defn x-y-viewer [cursor owner]
  (om/component
   (sab/html
    [:div
     "x: " (:x cursor) ", "
     "y: " (:y cursor)])))

(defn sin-viewer [cursor owner]
  (om/component
   (let [freq (:x cursor)
         ampl (:y cursor)]
     (sab/html
      (graph freq ampl)))))

;; Final display
(def app-model
  (atom {:scrubbable {:x 50 :y 15}}))

;; Note: following will trigger React warning: react-warning-keys. Safe to ignore.
(defcard edn-card-shared
  (dc/edn-card app-model))

(defcard om-generic-scrubber
  (dc/om-root-card (partial scrubbable-widget "x-y-view" x-y-viewer) app-model))

(defcard om-sin-view
  (dc/om-root-card (partial scrubbable-widget "sine-view" sin-viewer) app-model))
