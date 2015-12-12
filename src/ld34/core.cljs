(ns ^:figwheel-always ld34.core
    (:require [infinitelives.pixi.canvas :as canv]
              [infinitelives.pixi.resources :as resources]
              [infinitelives.pixi.sprite :as sprite]
              [infinitelives.utils.events :as events]
              [infinitelives.utils.console :refer [log]]
              [infinitelives.pixi.texture :as texture]
              [infinitelives.pixi.font :as font]
              [infinitelives.utils.math :as math]
              [infinitelives.utils.vec2 :as vec2]
              [infinitelives.utils.sound :as sound]
              [infinitelives.utils.dom :as dom]
              [infinitelives.procedural.maps :as maps]
              [ld34.assets :as a]
              [ld34.shaders :as shaders]
              [ld34.boid :as boid]
              [cljs.core.async :refer [<! chan put!]]
              [PIXI])
    (:require-macros [cljs.core.async.macros :refer [go]]
                     [ld34.macros :as m]
                     ))

(enable-console-print!)

(set! (.-scaleModes.DEFAULT js/PIXI) (.-scaleModes.NEAREST js/PIXI))

(defonce canvas
  (canv/init
   {:expand true
    :engine :auto
    :layers [:bg :world :ui]
    :background 0xa2d000 ;0x505050
    }))

(defonce render-go-block (go (while true
               (<! (events/next-frame))
               ((:render-fn canvas)))))

(defonce load-fonts (font/google ["Indie Flower"]))


(defonce font-inconsolata (font/make-tiled-font "Indie Flower" 100 10))
(defonce test-text (font/make-text "500 24px Indie Flower"
                                    "Let It Grow\n"
                                    :weight 500
                                    :fill "#ff00ff"
                                    ;:stroke "#505050"
                                    ;:strokeThickness 1
                                    ))

(defonce ui-state
  (atom
   {:screen-pos [0 0]}))



(def mouse-down (chan 1))
(def mouse-up (chan 1))
(def mouse-move (chan 1))

(defn on-mouse-down [ev]
  (put! mouse-down [ev (.-clientX ev) (.-clientY ev)]))

(defn on-mouse-up [ev]
  (put! mouse-up [ev (.-clientX ev) (.-clientY ev)]))

(defn on-mouse-move [ev]
  (put! mouse-move [ev (.-clientX ev) (.-clientY ev)]))

(.addEventListener js/window "mousedown" on-mouse-down true)
(.addEventListener js/window "mouseup" on-mouse-up)
(.addEventListener js/window "mousemove" on-mouse-move)

(defn ui-control [setpos-fn]
  (go
    (while true
      ;; wait for mouse down
      (let [[[ev ox oy] c] (alts! #js [mouse-move mouse-up mouse-down])]
        (cond
          (and (= c mouse-down)
               (= 2 (.-button ev)))
          (let [[start-x start-y] (:screen-pos @ui-state)]
            (log "mouse-down")

            ;; loop until mouse up
            (loop [x 0 y 0]
              (let [[[ev x y] c2] (alts! #js [mouse-up mouse-move])]
                (cond
                  (= c2 mouse-up)
                  (do (log "mouse-up")
                      (setpos-fn (- start-x (- x ox)) (- start-y (- y oy)))
                      (swap! ui-state assoc :screen-pos [(- start-x (- x ox)) (- start-y (- y oy))]))

                  (= c2 mouse-move)
                  (do (log "mouse-move")
                      (setpos-fn (- start-x (- x ox)) (- start-y (- y oy)))
                      (recur x y))
                  )
                )
              )
            )))
        ))
)





(defn depth-compare [a b]
  (cond
    (< (.-position.y a) (.-position.y b)) -1
    (< (.-position.y b) (.-position.y a)) 1
    :default 0
    ))


(defn main []
  (go
    ;; load assets with loader bar
    (<! (resources/load-resources
         (-> canvas :layer :ui)
         [
          "sfx/select-man.ogg"
          "img/sprites.png"
                                        ;"img/stars.png"
          "http://fonts.gstatic.com/s/indieflower/v8/10JVD_humAd5zP2yrFqw6ugdm0LZdjqr5-oayXSOefg.woff2"
          ]
         :full-colour 0x807030
         :highlight 0xffff80
         :lowlight 0x303010
         :empty-colour 0x202020
         :debug-delay 0.2
         :width 400
         :height 32
         :fade-in 0.2
         :fade-out 0.5))

    ;;
    ;; play music on a loop
    ;;
    #_ (go (let [tune (<! (sound/load-sound "/sfx/splash-screen.ogg"))
                 [source gain] (sound/play-sound tune 0.7 true)]))

    (let [sheet (resources/get-texture :sprites :nearest)
          assets (into {}
                       (for [[key {:keys [pos size]}] a/assets]
                         [key
                          (texture/sub-texture sheet pos size)
                          ]))
          scale 3.0

          map-width 50
          map-height 50
          half-map-width 25
          half-map-height 25
          all-cells (for [x (range map-width) y (range map-height)]
                      [x y])
          cell-width 200
          cell-height 200
          half-cell-width 100
          half-cell-height 100
          level-map (maps/make-rpg-map map-width map-height 1)

          sfx-select-man (<! (sound/load-sound "/sfx/select-man.ogg"))
          ]

      (sprite/set-alpha! test-text 0.0)
      (sprite/set-scale! test-text 5)
      (set! (.-filters test-text) #js [
                                        ;(shaders/make-colour-bars)
                                       (shaders/make-wavy)
                                       (shaders/make-colour-bars)])
      (.addChild (-> canvas :layer :ui) test-text)
      (resources/fadein test-text :duration 5)
      (go (loop [n 0]
            (let [h (.-innerHeight js/window)
                  hh (/ h 2)
                  qh (/ h 4)]
              (sprite/set-pos! test-text 0 ;-200
                               (+ (- n) (- (* 0.1 qh (Math/sin (* 0.1 n))) qh) 1000)
                               ))
            (<! (events/next-frame))
            (recur (inc n))))



      (m/with-sprite-set canvas :bg
        [tests (filter identity (for [cell all-cells
                                     n (range 3)]
                                  (let  [[x y] cell]
                                    (if (and (not= "water" (level-map cell))
                                             (not= "sand" (level-map cell)))
                                      (sprite/make-sprite
                                       ((rand-nth [:ground-1 :ground-2]) assets)
                                       :scale [(* 2 scale) (* 2 scale)]
                                       :x (math/rand-between (- (* cell-width (- x half-map-width)) half-cell-width)
                                                             (+ (* cell-width (- x half-map-width))
                                                                half-cell-width))
                                       :y (math/rand-between (- (* cell-height (- y half-map-height)) half-cell-height)
                                                             (+ (* cell-height (- y half-map-height))
                                                                half-cell-height))
                                       :xhandle 0.5 :yhandle 0.5
                                       :alpha 1.0)))))]

        (m/with-sprite-set canvas :bg
          [grass (filter identity
                         (for [cell all-cells
                               n (range 2)]
                           (let [[x y] cell]
                             (when (or
                                    (= "grass" (level-map cell))
                                    (= "trees" (level-map cell)))
                               (sprite/make-sprite
                                ((rand-nth [:grass-1 :grass-2 :grass-3]) assets)
                                :scale [(* 2 scale) (* 2 scale)]
                                :x (math/rand-between (- (* cell-width (- x half-map-width)) half-cell-width)
                                                      (+ (* cell-width (- x half-map-width))
                                                         half-cell-width))
                                :y (math/rand-between (- (* cell-height (- y half-map-height)) half-cell-height)
                                                      (+ (* cell-height (- y half-map-height))
                                                         half-cell-height))
                                :xhandle 0.5 :yhandle 0.5
                                :alpha 1.0)))
                           ))]

          (m/with-sprite-set canvas :world
            [trees (filter identity
                           (for [cell all-cells
                                 n (range 5)]
                             (let [[x y] cell]
                               (when (= "trees" (level-map cell))
                                 (sprite/make-sprite
                                  ((rand-nth [:tree-1 :tree-2 :tree-3 :tree-4 :tree-5 :tree-6]) assets)
                                  :scale [(* 2 scale) (* 2 scale)]
                                  :x (math/rand-between -3000 3000)
                                  :y (math/rand-between -3000 3000)
                                  :xhandle 0.5 :yhandle 1.0
                                  :alpha 1.0
                                  )))))]
            (m/with-sprite-set canvas :bg
              [sand (filter identity
                           (for [cell all-cells
                                 n (range 2)]
                             (let [[x y] cell]
                               (when (= "sand" (level-map cell))
                                 (sprite/make-sprite
                                  ((rand-nth [:sand-1 :sand-2 :sand-3 :sand-4]) assets)
                                  :scale [(* 2 scale) (* 2 scale)]
                                  :x (math/rand-between -3000 3000)
                                  :y (math/rand-between -3000 3000)
                                  :xhandle 0.5 :yhandle 1.0
                                  :alpha 1.0
                                  )))))]
              (m/with-sprite canvas :world
                [walker (sprite/make-sprite
                         (:char-1 assets)
                         :scale [(* 2 scale) (* 2 scale)]
                         :x 0 :y 0
                         :xhandle 0.5 :yhandle 1.0
                         :alpha 1.0
                         )]

                (let [bg-chan (chan)
                      dest (atom (vec2/vec2 0.1 0.1))]
                  (set! (.-interactive walker) true)
                  (set! (.-mousedown walker) (fn [ev] (log "mousedown" ev)))

                  (set! (.-interactive (-> canvas :stage)) true)
                  (set! (.-mousedown (-> canvas :stage)) (fn [ev] (log "MD" ev)))

                  (set! (.-interactive (-> canvas :layer :bg)) true)
                  (set! (.-mousedown (-> canvas :layer :bg))
                        (fn [ev] (log "BG" ev)
                          (log (.-originalEvent.clientX ev) (.-originalEvent.clientY ev))
                          (go
                            (sound/play-sound sfx-select-man 0.5 false)
                            (let [x (- (.-originalEvent.clientX ev)
                                       (/  (.-width (:canvas canvas)) 2))
                                  y (- (.-originalEvent.clientY ev)
                                       (/ (.-height (:canvas canvas)) 2))]
                              (m/with-sprite canvas :ui
                                [click
                                 (sprite/make-sprite
                                  (:click-mark-1 assets)
                                  :scale [(* 2 scale) (* 2 scale)]
                                  :x x :y y
                                  :xhandle 0.45 :yhandle 0.6)]

                                (<! (events/wait-time 100)))
                              (m/with-sprite canvas :ui
                                [click
                                 (sprite/make-sprite
                                  (:click-mark-2 assets)
                                  :scale [(* 2 scale) (* 2 scale)]
                                  :x x :y y
                                  :xhandle 0.45 :yhandle 0.6)]

                                (<! (events/wait-time 100))))
                            )
                          (reset! dest
                                  (vec2/add
                                   (let [[x y] (:screen-pos @ui-state)]
                                     (vec2/vec2 x y))
                                   (vec2/vec2
                                    (- (.-originalEvent.clientX ev)
                                       (/  (.-width (:canvas canvas)) 2))
                                    (- (.-originalEvent.clientY ev)
                                       (/ (.-height (:canvas canvas)) 2)))))
                                        ;(put! bg-chan ev)
                          ))

                  (set! (.-onmousedown (:canvas canvas))
                        (fn [e] (when (= 2 (.-button e)) (log "right"))))

                  (set! (.-oncontextmenu (:canvas canvas))
                        (fn [e] (.preventDefault e)))

                  (.sort (.-children (-> canvas :layer :world)) depth-compare )


                  ;; start ui control thread
                  (ui-control
                   (fn [x y]
                     (let [xp x
                           yp y]
                       (sprite/set-pivot! (-> canvas :layer :world) xp yp)
                       (sprite/set-pivot! (-> canvas :layer :bg) xp yp))
                     )
                   )




                  (loop [b {:mass 5.0
                            :pos (vec2/vec2 0 0)
                            :vel (vec2/vec2 0 0)
                            :max-force 2.0
                            :max-speed 2.0}]
                    (.sort (.-children (-> canvas :layer :world)) depth-compare )
                    (<! (events/next-frame))

                                        ;(log (str b))

                    (let [nb (boid/arrive b @dest 30.0)]
                                        ;(log (str nb))
                      (sprite/set-pos! walker (:pos nb))
                      (recur nb)
                      )

                    ))

                                        ;(<! (events/wait-time 300000))
                ))))

        )


      )

    ))

(main)






(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
