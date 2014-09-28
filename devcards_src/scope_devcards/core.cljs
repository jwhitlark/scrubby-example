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

;; -------------------- Helpers --------------------

(defn by-id [id]
  (.getElementById js/document id))

(defn set-states! [owner desired]
  (doall (map #(om/set-state! owner (key %) (val %)) desired)))

;; -------------------- Helpers --------------------

(defn calc-start [amp]
  (str  "M" "0.1,"  (- 15 amp)))

(defn sin-seq
  "Given a frequency, generate a sequence to use as the :d value of an svg path element."
  [freq amp]
  ;; FIXME: Need to scale these so that low values don't cause jaggies  (jw 14-09-27)
  (str/join " "
         (cons (calc-start amp)
          (for [x (range (* 2 freq))]
            (str "l" (/ 50 freq) ","
                 (* amp (.sin js/Math x)))))))

(defn graph [freq ampl]
  [:div [:svg
         {:style {:width "100px" :height "30px"} }
         ;; the frame around the graph
         [:path {:cs "100,100"
                 :d "M0.0,0.0 L99.5,0.0 L99.5,29.5 L0.5,29.5 Z"
                 :stroke "blue" :stroke-width "1"
                 :fill "white" :fill-opacity "0"}]
         ;; graph itself
         [:path {:cs "100,100"
                 :d (sin-seq freq ampl)
                  :fill "none" :stroke-width "1" :stroke-opacity "1"
                 :stroke "green"}]
         ]])

(defn simplify-event
  [evt]
  {:x (.-clientX evt)
   :y (.-clientY evt)
   :type (.-type evt)
   :e evt})

(def buffer-max 100)

(defn log [x]
  (.log js/console x))

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
      (let [mouse-chan (om/get-state owner :mouse-chan)]
        (go-loop []
          (let [[evt-type e] (<! mouse-chan)]
            (handle-scrubber-evt cursor owner evt-type e))
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
                       :style {:z-index 100
                               :color (if (:capturing state) "#00f" "#000")
                               :border-bottom "1px dotted #00f"
                               :cursor "all-scroll"
                               :-webkit-user-select "none"}}
                 build-fn]))))


(defn scrubbable-widget [id target cursor owner]
     (reify
       om/IRenderState
       (render-state [_ state]
         (sab/html
          [:div {:class "container"}
           (om/build scrubbable
                     (:scrubbable cursor)
                     {:opts {:id id
                             :build-fn (om/build target (:scrubbable cursor))}})]))))

(defn x-y-viewer [cursor owner]
  (om/component
   (sab/html
    [:div [:em "x: " (:x cursor) ", "]
     [:em "y: " (:y cursor)]])))

(defn sin-viewer [cursor owner]
  (om/component
   (let [freq (:x cursor)
         ampl (:y cursor)]
     (sab/html
      (graph freq ampl)))))

(def app-model
  (atom {:scrubbable {:x 50 :y 15}}))

(defcard om-generic-scrubber
  (dc/om-root-card (partial scrubbable-widget "x-y-view" x-y-viewer) app-model))

(defcard om-sin-view
  (dc/om-root-card (partial scrubbable-widget "sine-view" sin-viewer) app-model))

(defcard edn-card-shared
  (dc/edn-card app-model))
