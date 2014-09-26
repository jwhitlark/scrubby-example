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

;; -------------------- Helpers --------------------

(defn listen
  ;; TODO: Factor chan creation out (jw 14-09-26)
  ([el types]
     (let [out (chan)]
       (doall (map (fn [type] (events/listen el type #(put! out %))) types))
       out))
  ([el buffer-max types]
     (let [out (chan (async/sliding-buffer buffer-max))]
       (doall (map (fn [type] (events/listen el type #(put! out %))) types))
       out))
  )

(defn set-states! [owner desired]
  (doall (map #(om/set-state! owner (key %) (val %)) desired)))

;; -------------------- Helpers --------------------

(defn sin-seq
  "Given a frequency, generate a sequence to use as the :d value of an svg path element."
  [freq]
  ;; TODO: Need to scale these so that low values don't cause jaggies  (jw 14-09-26)
  (apply str (for [x (range (* 2 freq))]
               (str "l" (/ 50 freq) ","
                    (* 15 (.sin js/Math x)) " "))))

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

(defn simplify-event
  [evt]
  {:x (.-clientX evt)
   :type (.-type evt)})

(defn reset-scrubber []
  {:capturing false
   :start-x nil})

(defn sin-scrubber [app owner]
  (reify
    om/IInitState
    (init-state [_]
      (reset-scrubber))
    om/IWillMount
    (will-mount [_]
      (let [mouse-chan (async/map simplify-event
                                  [(listen js/window 100 [EventType.MOUSEMOVE
                                                          EventType.MOUSEUP])])]
        (go (while true?
              (let [evt (<! mouse-chan)]
                (if (om/get-state owner :capturing)
                  (case (:type evt)
                    "mousemove" (let [difference (- (:x evt) (om/get-state owner :start-x))]
                                  (om/transact! app :freq (partial + difference))
                                  (om/set-state! owner :start-x (:x evt)))
                    "mouseup" (set-states! owner (reset-scrubber))
                    (do (.log js/console "Got unexpected evt")
                        (.log js/console evt)))))))))
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
