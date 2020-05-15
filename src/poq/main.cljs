(ns poq.main
  (:require
    [cljs.core.async :refer (chan put! close! <! go timeout) :as async]
    [cljs.core.async.interop :refer-macros [<p!]]
    [alandipert.storage-atom :refer [local-storage]]
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [shadow.resource :as rc]))

(defonce state (local-storage (r/atom {}) :state))

(defn update-loop! [state]
  (js/alert "update loop\n" (str @state)))

(defn editable-wrapper [state props]
  (let [v (atom (@state :bpm))]
    (assoc props
           :on-blur (js/alert "out")
           :value (or @v 180))))

(defn update-val! [state k ev]
  (swap! state
         assoc k (-> ev .-target .-value)))

(defn component-main [state]
  (let [bpm (-> (or (@state :bpm) 180) int (min 200) (max 0))
        swing (-> (or (@state :swing) 0) int (min 100) (max 0))]
  [:div
   [:div.input-group
    [:label
     [:input
      {:type "range"
       :min 30
       :max 200
       :on-change (partial update-val! state :bpm)
       :on-mouse-up (partial update-loop! state)
       :on-touch-end (partial update-loop! state)
       :value bpm}]
     "tempo"]]
   [:div.input-group
    [:input#bpm
     {:type "number"
      :on-change (partial update-val! state :bpm)
      :on-blur (partial update-loop! state)
      :on-key-up #(if (= (.-keyCode %) 13) (-> % .-target .blur))
      :on-input #(when (not= (aget js/document "activeElement") (.-target %))
                  (update-loop! state))
      :value bpm}]]
   [:div.input-group
    [:label
     [:input {:type "range"
              :min 0
              :max 100
              :on-change (partial update-val! state :swing)
              :on-mouse-up (partial update-loop! state)
              :on-touch-end (partial update-loop! state)
              :value swing}]
     "swing"]]
   [:div.input-group
    [:button "play"]
    [:button "tap"]]]))

(defn reload! []
  (rdom/render [component-main state] (js/document.getElementById "app")))

(defn main! []
  (go
    (<! (timeout 1000))
    (swap! state assoc :audio-context (js/AudioContext.))
    (reload!)))
