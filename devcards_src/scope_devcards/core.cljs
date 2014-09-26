(ns scope-devcards.core
    (:require
     [devcards.core :as dc :include-macros true]
     [scope.core :as scope]
     [om.core :as om :include-macros true]
     [om.dom :as dom :include-macros true]
     [sablono.core :as sab :include-macros true]
     [goog.events :as events]
     [cljs.core.async :as async :refer [>! <! put! chan]])
    (:require-macros
     [devcards.core :refer [defcard is are= are-not=]]
     [cljs.core.async.macros :refer [go]])
    (:import [goog.events EventType]))

(enable-console-print!)

(devcards.core/start-devcard-ui!)
(devcards.core/start-figwheel-reloader!)

;; remember to run lein figwheel and then browse to
;; http://localhost:3449/devcards/index.html

#_ (defcard devcard-intro
  (dc/markdown-card
   "# Devcards for scope

    I can be found in `devcards_src/scope_devcards/core.cljs`.

    If you add cards to this file, they will appear here on this page.

    You can add devcards to any file as long as you require
    `devcards.core` like so:

    ```
    (:require [devcards.core :as dc :include-macros true])
    ```

    As you add cards to other namspaces, those namspaces will
    be listed on the Devcards **home** page.

    <a href=\"https://github.com/bhauman/devcards/blob/master/example_src/devdemos/core.cljs\" target=\"_blank\">Here are some Devcard examples</a>"))

#_ (defn widget [data owner]
  (reify
    om/IRender
    (render [this]
      (sab/html [:h2 "This is an om card, " (:text data)]))))

#_ (defcard omcard-ex
    (dc/om-root-card widget {:text "yozers"}))

(defn listen [el & types]
  (let [out (chan)]
    (doall (map (fn [type] (events/listen el type #(put! out %))) types))
    out))

(defn set-states! [owner desired]
  (doall (map #(om/set-state! owner (key %) (val %)) desired)))

(defn scrubbing-int-app-event-view-2 [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:capturing false
       :start-x nil})
    om/IWillMount
    (will-mount [_]
      (let [mouse-chan (async/map (fn [e] {:x (.-clientX e)
                                           :type (.-type e)})
                        [(listen js/window EventType.MOUSEMOVE EventType.MOUSEUP)])]
        (go (while true?
              (let [evt (<! mouse-chan)]
                (if (om/get-state owner :capturing)
                  (case (:type evt)
                    "mousemove" (let [difference (- (:x evt) (om/get-state owner :start-x))]
                                  (om/transact! app :my-val (partial + difference))
                                  (om/set-state! owner :start-x (:x evt)))
                    "mouseup" (set-states! owner {:capturing false :start-x nil}))))))))
    om/IRenderState
    (render-state [_ state]
      (dom/div nil
               (dom/span #js { :style #js {:color (if (:capturing state) "#00f" "#000")
                                           :border-bottom "1px dotted #00f"
                                           :cursor "col-resize"
                                           :-webkit-user-select "none"}
                              :onMouseDown #(set-states! owner {:capturing true :start-x (.-clientX %)})}
                         (str "Current state is: " state ", Current app val is: " (:my-val app)))))))

(defcard om-int-app-event-scrubber-2
  (dc/om-root-card scrubbing-int-app-event-view-2 {:my-val 0}))

(defn sin-seq [f]
  (apply str (for [x (range (* 2 f))]
               (str "l" (/ 50 f) "," (* 15 (.sin js/Math x))  " "))))

(defn graph [freq]
  [:div [:svg
         {:style {:width "100px" :height "30px"} }
         [:path {:cs "100,100"
                 :d "M0.0,0.0 L99.5,0.0 L99.5,29.5 L0.5,29.5 Z" ;; the box
                 :stroke "blue" :stroke-width "1"
                 :fill "white" :fill-opacity "0"}]
         [:path {:cs "100,100"
                 :d (str "M0.1,0.5" (sin-seq freq))
                 :fill "none" :stroke-width "1" :stroke-opacity "1"
                 :stroke "green"}]
         ]]
  )

(defn sin-scrubber [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:capturing false
       :start-x nil})
    om/IWillMount
    (will-mount [_]
      (let [mouse-chan (async/map (fn [e] {:x (.-clientX e)
                                           :type (.-type e)})
                                  [(listen js/window EventType.MOUSEMOVE EventType.MOUSEUP)])]
        (go (while true?
              (let [evt (<! mouse-chan)]
                (if (om/get-state owner :capturing)
                  (case (:type evt)
                    "mousemove" (let [difference (- (:x evt) (om/get-state owner :start-x))]
                                  (om/transact! app :freq (partial + difference))
                                  (om/set-state! owner :start-x (:x evt)))
                    "mouseup" (set-states! owner {:capturing false :start-x nil}))))))))
    om/IRenderState
    (render-state [_ state]
      (sab/html [:span
                 "Frequency of sine wave: "
                 [:span {:style {:color (if (:capturing state) "#00f" "#000")
                                 :border-bottom "1px dotted #00f"
                                 :cursor "col-resize"
                                 :-webkit-user-select "none"}
                         :onMouseDown #(set-states! owner {:capturing true
                                                           :start-x (.-clientX %)})}
                  (str (:freq app))]
                 (graph (:freq app))]
                ))))

(defcard om-sin-scrubber
  (dc/om-root-card sin-scrubber {:freq 25}))
