(ns poq.main
  (:require
    [shadow.resource :as rc]
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [alandipert.storage-atom :refer [local-storage]]
    [dopeloop.main :refer [audio-context seamless-loop-audio-buffer! stop-source!]]))

(def initial-state {:bpm 180 ; persisted
                    :swing 0 ; persisted
                    :playing false
                    :taps []
                    :audio-source nil
                    :context nil
                    :show-menu false})

(def bpm-min 60)
(def bpm-max 240)

(def local-storage-keys [:bpm :swing])

(def buttons {:play (rc/inline "sprites/button-play.svg")
              :stop (rc/inline "sprites/button-stop.svg")
              :bars (rc/inline "sprites/bars.svg")
              :exex (rc/inline "sprites/times.svg")})

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
         assoc-in k (if (number? ev) ev (int (-> ev .-target .-value)))))

(defn set-bpm! [state v]
  (update-val! state [:bpm] v)
  (update-loop! state))

(defn get-bpm [*state]
  (-> *state :bpm int (min bpm-max) (max bpm-min)))

(defn get-swing [*state]
  (-> *state :swing int (min 100) (max 0)))

(defn component-icon [svg]
  [:span.icon {:ref (fn [el] (when el (aset el "innerHTML" svg)))}])

(defn component-slider [k value min-val max-val]
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

(defn component-menu-toggle [state]
  [:div#menu.input-group
   [:span {:on-click #(swap! state update :show-menu not)}
    [component-icon (if (:show-menu @state) (:exex buttons) (:bars buttons))]]])

(defn component-help [state]
  [:div
   [component-menu-toggle state]
   [:div#help "Hello goose hello hello yes goose hello hello yes goose hello hello yes goose hello hello yes goose."]
   [:div]])

(defn component-main [state]
  (let [bpm (get-bpm @state)
        swing (get-swing @state)
        playing (@state :playing)]
    [:div
     [component-menu-toggle state]
     [:div#tempo.input-group
      [:button {:disabled (< (/ bpm 2) bpm-min)
                :on-click #(set-bpm! state (/ bpm 2))} "½"]
      [:span {:class (when (<= bpm bpm-min) "disabled")
              :on-click #(set-bpm! state (- bpm 1))} "–"]
      [:div#bpm bpm]
      [:span {:class (when (>= bpm bpm-max) "disabled")
              :on-click #(set-bpm! state (+ bpm 1))} "+"]
      [:button {:disabled (> (* bpm 2) bpm-max)
                :on-click #(set-bpm! state (* bpm 2))} "2"]]
     [:div.input-group
      [component-slider :bpm bpm bpm-min bpm-max]]
     [:div.input-group
      [:button#tap {:on-click #(tap! state)} "tap"]]
     [:div.input-group
      [component-slider :swing swing 0 75]]
     [:div.input-group
      [:button#play {:on-click #(if playing (stop! state) (play! state))
                     :ref (fn [el]
                            (when el
                              (aset el "innerHTML" (if playing (:stop buttons) (:play buttons)))))}]]]))

(defn component-pages [state]
  (if (:show-menu @state)
    [component-help state]
    [component-main state]))

(defn reload! {:dev/after-load true} []
  (rdom/render [component-pages state] (js/document.getElementById "app")))

(defn main! []
  (swap! state assoc :context (audio-context.))
  (reload!))
