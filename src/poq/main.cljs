(ns poq.main
  (:require
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [alandipert.storage-atom :refer [local-storage]]
    [dopeloop.main :refer [audio-context seamless-loop-audio-buffer! stop-source!]]))

(def initial-state {:bpm 180 ; persisted
                    :swing 0 ; persisted
                    :playing false
                    :taps []
                    :audio-source nil
                    :context nil})

(def bpm-min 60)
(def bpm-max 240)

(def local-storage-keys [:bpm :swing])

(defonce state (local-storage (r/atom initial-state)
                              :pocketsync-settings
                              (fn [*state]
                                (print "pre" *state)
                                (select-keys *state local-storage-keys))
                              (fn [*state *old-state]
                                (print "post" *state *old-state)
                                (merge initial-state *state))))

(defn make-click-track-audio-buffer [{:keys [context bpm swing] :as *state}]
  (let [beat-seconds (/ (/ 60 bpm) 2)
        beats 2
        sample-rate (aget context "sampleRate")
        frames-per-beat (int (* beat-seconds sample-rate))
        frame-count (* beats frames-per-beat)
        swing-frames (-> swing (/ 100) (* frames-per-beat))
        buffer (.createBuffer context 2 frame-count sample-rate)]
    (doseq [b [0]]
      (let [array-buffer (.getChannelData buffer b)]
        (doseq [beat (range beats) i (range frames-per-beat)]
          (aset array-buffer
                (+ i (* beat frames-per-beat))
                (if
                  (if (= (mod beat 2) 0)
                    (< i 882)
                    (and (< (- i swing-frames) 882)
                         (> i swing-frames)))
                  1.0
                  -0.01)))))
    (assoc *state :audio-buffer buffer)))

(defn play-click-track! [{:keys [context audio-buffer audio-source] :as *state}]
  (assoc *state :audio-source
         (seamless-loop-audio-buffer! context audio-buffer audio-source)))

(defn update-loop! [state]
  (let [{:keys [playing]} @state]
    (when playing
      (swap! state
             #(-> % make-click-track-audio-buffer play-click-track!)))))

(defn play! [state]
  (swap! state assoc :playing true)
  (update-loop! state))

(defn stop! [state]
  (let [click-track-audio-source (@state :audio-source)]
    (swap! state dissoc :playing :audio-source :audio-buffer)
    (stop-source! click-track-audio-source)))

(defn average [coll]
  (/ (reduce + coll) (count coll)))

(defn new-tap [*state]
  (let [taps (-> *state :taps)
        bpm (-> *state :bpm)
        taps (if (seq? taps) taps [])
        now (.getTime (js/Date.))
        taps (conj (if (seq? taps) taps []) now)
        taps (filter #(> % (- now 3000)) taps)
        tap-threshold (> (count taps) 3)
        tap-diffs (reduce
                    (fn [[last-tap accum] tap]
                      [tap
                       (if (= last-tap tap)
                         accum
                         (conj accum (- last-tap tap)))])
                    [now []]
                    taps)
        avg-tap-length (average (second tap-diffs))
        bpm (if tap-threshold
              (int (/ 60000 avg-tap-length))
              bpm)]
    (assoc *state :bpm bpm :taps taps)))

(defn tap! [state]
  (let [previous-state @state
        updated-state (swap! state new-tap)]
    (when (not= updated-state previous-state)
      (js/console.log "CHANGED")
      (update-loop! state))))

(defn update-val! [state k ev]
  (swap! state
         assoc-in k (int (-> ev .-target .-value))))

(defn bpm-to-degrees [bpm]
  (let [normalized (-> bpm (- bpm-min) (/ (- bpm-max bpm-min)))]
    (-> normalized
      (* 350)
      (- 85))))

(defn get-bpm [*state]
  (-> *state :bpm int (min bpm-max) (max bpm-min)))

(defn component-bpm-dial [state]
  (let [touchstate (atom nil)]
    (fn []
      (let [bpm (get-bpm @state)]
        [:svg#tempo {:viewBox "-100 -100 200 200"
                     :on-mouse-down #(reset! touchstate true)
                     :on-mouse-out #(reset! touchstate false)
                     :on-mouse-up #(reset! touchstate false)
                     :on-mouse-move #(when @touchstate (js/console.log %))}
         [:text {:x 0 :y -40 :text-anchor "middle" :dominant-baseline "middle"} bpm]
         [:g {:transform (str "rotate(" (bpm-to-degrees bpm) " 0 0)")}
          [:g {:transform "translate(85 0)"}
           [:circle {:cx 0 :cy 0 :r 10 :stroke-width "2px"}]]]
         [:circle {:cx 0 :cy 0 :r 85 :fill "transparent" :path-length 5}]
         (for [b (range bpm-min (inc bpm-max) 10)]
           [:g {:transform (str "rotate(" (bpm-to-degrees b) " 0 0)")
                :key (str "tick-" b)}
            [:line {:x1 85 :y1 0 :x2 90 :y2 0}]])]))))

(defn component-main [state]
  (let [bpm (get-bpm @state)
        swing (-> @state :swing int (min 100) (max 0))
        playing (@state :playing)]
    [:div
     [:div.input-group
      [:label
       [:input
        {:type "range"
         :min bpm-min
         :max bpm-max
         :on-change (partial update-val! state [:bpm])
         :on-mouse-up #(update-loop! state)
         :on-touch-end #(update-loop! state)
         :value bpm}]]]
     [component-bpm-dial state]
     [:button#tap {:on-click #(tap! state)} "tap"]
     [:div.input-group
      [:label
       [:input {:type "range"
                :min 0
                :max 75
                :on-change (partial update-val! state [:swing])
                :on-mouse-up #(update-loop! state)
                :on-touch-end #(update-loop! state)
                :value swing}]]]
     [:div.input-group
      [:button {:on-click #(if playing (stop! state) (play! state))} "stop"]]]))

(defn reload! {:dev/after-load true} []
  (rdom/render [component-main state] (js/document.getElementById "app")))

(defn main! []
  (swap! state assoc :context (audio-context.))
  (reload!))
