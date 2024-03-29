(ns poq.main
  (:require
    [shadow.resource :as rc]
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [alandipert.storage-atom :refer [local-storage]]
    [dopeloop.main :refer [audio-context
                           seamless-loop-audio-buffer!
                           stop-source!
                           get-loop-position
                           manage-audio-context-ios
                           poll-device-volume on-ios?
                           lock-screen-orientation
                           on-device-ready
                           wake-screen-lock]]))

(def initial-state {:bpm 90 ; persisted
                    :swing 0 ; persisted
                    :playing false
                    :device-volume 1
                    :taps []
                    :audio-source nil
                    :context nil
                    :show-menu false
                    :playback-position nil})

(def bpm-range [60 240])
(def swing-range [0 75])

(def local-storage-keys [:bpm :swing])

(def buttons {:play (rc/inline "sprites/button-play.svg")
              :stop (rc/inline "sprites/button-stop.svg")
              :bars (rc/inline "sprites/bars.svg")
              :exex (rc/inline "sprites/times.svg")})

(defonce state (local-storage (r/atom initial-state)
                              :pocketsync-settings
                              (fn [*state]
                                (select-keys *state local-storage-keys))
                              (fn [*state]
                                (merge initial-state *state))))

; *** non-mutating functions *** ;

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


(defn get-bpm [*state]
  (-> *state :bpm int (min (last bpm-range)) (max (first bpm-range))))

(defn get-swing [*state]
  (-> *state :swing int (min (last swing-range)) (max (first swing-range))))

; *** mutations *** ;

(defn make-click-track-audio-buffer! [{:keys [context bpm swing] :as *state}]
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
             #(-> % make-click-track-audio-buffer! play-click-track!)))))

(defn play! [state]
  (wake-screen-lock true)
  (swap! state assoc :playing true :context (audio-context.))
  (update-loop! state))

(defn stop! [state]
  (wake-screen-lock false)
  (let [click-track-audio-source (@state :audio-source)]
    (.close (:context @state))
    (swap! state dissoc :playing :audio-source :audio-buffer :context :playback-position)
    (stop-source! click-track-audio-source)))

(defn poll-playback-position! [state ms callback]
  (js/setInterval
    (fn []
      (let [audio-context (:context @state)
            audio-source (:audio-source @state)]
        (when (and audio-context audio-source)
          (callback (get-loop-position audio-context audio-source)))))
    ms))

(defn tap! [state]
  (let [previous-state @state
        updated-state (swap! state new-tap)]
    (when (not= updated-state previous-state)
      (update-loop! state))))

(defn update-val! [state k ev]
  (swap! state
         assoc-in k (if (number? ev) ev (int (-> ev .-target .-value)))))

(defn update-loop-val! [state k update-fn]
  (swap! state update-in [k] update-fn)
  (update-loop! state))

; *** components *** ;

(defn component-icon [svg]
  [:span.icon {:ref (fn [el] (when el (aset el "innerHTML" svg)))}])

(defn component-slider [state k value min-val max-val]
  (let [midpoint (/ (+ min-val max-val) 2)]
    [:label
     [:span (when (< value midpoint) {:class "right"}) (name k)]
     [:input
      {:type "range"
       :min min-val
       :max max-val
       :on-change #(update-val! state [k] %)
       :on-mouse-up #(update-loop! state)
       :on-touch-end #(update-loop! state)
       :value value}]]))

(defn component-tick-tock [playback-position]
  (let [[d1 d2] playback-position]
      [:<>
       [:div#pinger {:class (when (and d1 d2 (or (< d1 0.150) (> d1 (- d2 0.025)))) "on")}]]))

(defn component-menu-toggle [state]
  [:div#menu
   [component-tick-tock (:playback-position @state)]
   [:span {:on-click #(swap! state update :show-menu not)}
    [component-icon (if (:show-menu @state) (:exex buttons) (:bars buttons))]]])

(defn component-help [state]
  [:div#help
   [:div
    [component-menu-toggle state]
    [:div
     [:p [:a {:href "https://dopeloop.ai" :target "_BLANK"} [:button "Get more apps"]]]
     [:h3 "Help"]
     [:p "Use this app with a pocket operator device."]
     [:p "Plug a 3.5mm stereo cable from this device into the left input of your pocket operator.
         The sync signal is sent over the audio cable."]
     [:p "Turn the volume all the way up on this device."]
     [:p "Set sync mode on your pocket operator to SY4 or SY5 (sync pass-through)
         using the top-right button + BPM."]
     [:p "Press play to start the sync signal."]
     (when (not (on-ios?))
       [:<>
        [:h3 "Tips"]
        [:p "Huawei phones and maybe others have a 'battery saving' mode which lowers the headphone level.
            This prevents sync from working. You can turn off this mode in your phone settings."]])
     [:h3 "Privacy"]
     [:p
      "The app does not access, collect, use, or share any of your personal data.
      We don't collect any personal information."]
     [:p [:a {:href "https://dopeloop.ai/app-privacy-policy.html"} "Full privacy policy"] "."]
     [:p [:button.ok {:on-click #(swap! state update :show-menu not)} "Ok"]]]]
   [:div]])

(defn component-inputs [state bpm swing playing device-volume]
    [:<>
     [:div#tempo.input-group
      [:button {:disabled (< (/ bpm 2) (first bpm-range))
                :on-click (fn [] (update-loop-val! state :bpm #(/ % 2)))} "½"]
      [:span.clickable {:class (when (<= bpm (first bpm-range)) "disabled")
                        :on-click #(update-loop-val! state :bpm dec)} "–"]
      [:div#bpm bpm]
      [:span.clickable {:class (when (>= bpm (last bpm-range)) "disabled")
                        :on-click #(update-loop-val! state :bpm inc)} "+"]
      [:button {:disabled (> (* bpm 2) (last bpm-range))
                :on-click (fn [] (update-loop-val! state :bpm #(* % 2)))} "2"]]
     [:div.input-group
      [component-slider state :bpm bpm (first bpm-range) (last bpm-range)]]
     [:div.input-group
      [:button#tap {:on-click #(tap! state)} "tap"]]
     [:div.input-group
      [:span.clickable {:class (when (<= swing (first swing-range)) "disabled")
                        :on-click #(update-loop-val! state :swing dec)} "–"]
      [component-slider state :swing swing (first swing-range) (last swing-range)]
      [:span.clickable {:class (when (>= swing (last swing-range)) "disabled")
                        :on-click #(update-loop-val! state :swing inc)} "+"]]
     [:div.input-group
      [:div.highlight.device-warning
       (when (< device-volume 0.9)
         "Set device volume to max for sync.")]
      [:button#play {:on-click #(if playing (stop! state) (play! state))
                     :ref (fn [el]
                            (when el
                              (aset el "innerHTML" (if playing (:stop buttons) (:play buttons)))))}]]])

(defn component-main [state]
  [:div#app
   [:div
    [component-menu-toggle state]
    (let [bpm (get-bpm @state)
          swing (get-swing @state)
          playing (@state :playing)
          device-volume (@state :device-volume)]
      [component-inputs state bpm swing playing device-volume])]])

(defn component-pages [state]
  (if (:show-menu @state)
    [component-help state]
    [component-main state]))

(defn reload! {:dev/after-load true} []
  (rdom/render [component-pages state]
               (-> js/document (.querySelector "main"))))

(defn main! []
  (manage-audio-context-ios #(:context @state))
  (poll-device-volume 250 #(swap! state assoc :device-volume %))
  (poll-playback-position! state 16 #(swap! state assoc :playback-position %))
  (on-device-ready #(lock-screen-orientation "portrait-primary"))
  (reload!))
