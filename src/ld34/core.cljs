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
              ;[clojure.string :as string]
              [ld34.assets :as a]
              [ld34.shaders :as shaders]
              [ld34.boid :as b]
              [ld34.caravan :as c]
              [ld34.walker :as w]
              [ld34.game :as g]
              [ld34.flies :as flies]
              [ld34.tree :as tree]
              [ld34.hippy :as hippy]
              [ld34.effects :as effects]
              [cljs.core.async :refer [<! chan put! alts!]]
              [PIXI])
    (:require-macros [cljs.core.async.macros :refer [go]]
                     [infinitelives.pixi.macros :as m]
                     ))

(enable-console-print!)

(set! (.-scaleModes.DEFAULT js/PIXI) (.-scaleModes.NEAREST js/PIXI))

(defonce canvas
  (canv/init
   {:expand true
    :engine :auto
    :layers [:bg :world :float :ui]
    :background 0xa2d000 ;0x505050
    }))

(defonce render-go-block (go (while true
               (<! (events/next-frame))
               ((:render-fn canvas)))))

(defonce load-fonts (font/google ["Share Tech Mono" "Sue Ellen Francisco"]))


(defonce font-sharetech (font/make-tiled-font "Share Tech Mono" 100 10))
(defonce font-francisco (font/make-tiled-font "Sue Ellen Francisco" 100 10))
(defonce test-text (font/make-text "500 24px Sue Ellen Francisco"
                                    "\nLet It Grow\n"
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
            ;(log "mouse-down")

            ;; loop until mouse up
            (loop [x 0 y 0]
              (let [[[ev x y] c2] (alts! #js [mouse-up mouse-move])]
                (cond
                  (= c2 mouse-up)
                  (do ;(log "mouse-up")
                      (setpos-fn (- start-x (- x ox)) (- start-y (- y oy)))
                      (swap! ui-state assoc :screen-pos [(- start-x (- x ox)) (- start-y (- y oy))]))

                  (= c2 mouse-move)
                  (do ;(log "mouse-move")
                      (setpos-fn (- start-x (- x ox)) (- start-y (- y oy)))
                      (recur x y)))))))))))


(defn depth-compare [a b]
  (cond
    (< (sprite/get-y a) (sprite/get-y b)) -1
    (< (sprite/get-y b) (sprite/get-y a)) 1
    :default 0))


(defn main []
  (go
    ;; load assets with loader bar
    (<! (resources/load-resources
         (-> canvas :layer :ui)
         [
          "sfx/select-man.ogg"
          "sfx/button-open.ogg"
          "sfx/button-close.ogg"
          "sfx/button-select.ogg"
          "sfx/tree-planted.ogg"
          "sfx/tree-not-planted.ogg"
          "sfx/tree-chop.ogg"
          "sfx/tree-harvest.ogg"
          "sfx/tree-grow.ogg"
          "sfx/tree-hurt.ogg"
          "sfx/bug-spray.ogg"
          "sfx/fly-attack.ogg"
          "sfx/fly-change.ogg"
          "sfx/music.ogg"
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

    (go (let [tune (<! (sound/load-sound "/sfx/music.ogg"))
              [source gain] (sound/play-sound tune 0.7 true)]))

    (let [sheet (resources/get-texture :sprites :nearest)
          _ (texture/load-sprite-sheet! sheet a/assets)

          scale 3.0
          scale-2 (* 2 scale)

          set-pos-relative!
          (fn [spr corner space]
            (let [h-width (/ (.-width (:canvas canvas)) 2)
                  h-height (/ (.-height (:canvas canvas)) 2)]
              (case corner
                :top-left
                (do (sprite/set-anchor! spr 0 0)
                    (sprite/set-pos! spr (- space h-width) (- space h-height))))))

          seed-text (font/make-text "500 12px Share Tech Mono"
                                    ""
                                    :weight 400
                                    :fill "#ffffff"
                                        ;:stroke "#505050"
                                    :strokeThickness 0.1
                                    )
          _ (sprite/set-scale! seed-text 3)
          _ (.addChild (-> canvas :layer :ui) seed-text)
          _ (set-pos-relative! seed-text :top-left 10 )

          set-seed-text
          (fn [seeds dollars]
            (set-pos-relative! seed-text :top-left 10 )
            (.setText seed-text (str "S: " seeds "\n$: " dollars))
            )

          _ (set-seed-text 3 0)

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

          new-plant
          (fn
            [pos]
            {
             :age 0
             :growth-rate 0.001         ;0.002         ; 0.001
             :id (keyword (gensym))
             :yield 1000
             :pos pos
             :sprite (sprite/make-sprite :plant-1
                                         :x (vec2/get-x pos)
                                         :y (vec2/get-y pos)
                                         :scale scale-2
                                         :xhandle 0.5
                                         :yhandle 1.0)})

          new-hippy
          (fn [pos]
            {:pos pos
             :sprite (sprite/make-sprite :hippy-left
                                         :x (vec2/get-x pos)
                                         :y (vec2/get-y pos)
                                         :scale scale-2
                                         :xhandle 0.5
                                         :yhandle 1.0)})

          _ (g/init set-seed-text flies/make)

          update-seeds
          (fn []
            (let [state (-> @g/game :seeds)]
              nil
              ))

          update-flies flies/update-fly
          update-tree-chop tree/update-chop
          chop-tree-go-thread tree/chop-tree-go-thread
          hippy-go-thread hippy/hippy-go-thread

          ;; TODO: exit this thread on death
          fly-go-thread flies/go-thread

          update-plants
          (fn [plants]
                                        ;(log "Update-plants:" (str plants))
            (doall (for [plant plants]
                     (sprite/set-texture! (:sprite plant)
                                          (nth
                                           [:plant-1
                                            :plant-2
                                            :plant-3
                                            :plant-4
                                            :plant-5
                                            :plant-6
                                            :plant-7]
                                           (max 0 (min 6 (int (:age plant))))))))
            (into #{}
                  (map #(-> %
                            (update :age
                                    (fn [x]
                                      (let [next (+ x (-> % :growth-rate))]
                                        (when (> (int next) (int x))
                                          (sound/play-sound :tree-grow 0.3 false))
                                        next))))
                       plants)))]

      ;; add the first flys
      (doall
       (for [fly (-> @g/game :flies)]
         (do
           (.addChild (-> canvas :layer :world) (:sprite fly))
           (fly-go-thread fly))))

      ;; add the first hippies
      (doall
       (for [hippy (-> @g/game :hippies)]
         (do
                                        ;(log "ADDING HIPPY" (:sprite hippy))
           (.addChild (-> canvas :layer :world) (:sprite hippy))
                                        ;(log "go thread")
           (hippy-go-thread g/game canvas hippy))))



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
                                       (rand-nth [:ground-1 :ground-2])
                                       :scale scale-2
                                       :x (math/rand-between (- (* cell-width (- x half-map-width)) half-cell-width)
                                                             (+ (* cell-width (- x half-map-width))
                                                                half-cell-width))
                                       :y (math/rand-between (- (* cell-height (- y half-map-height)) half-cell-height)
                                                             (+ (* cell-height (- y half-map-height))
                                                                half-cell-height))
                                       :xhandle 0.5 :yhandle 0.5
                                       :alpha 1.0)))))

         grass (filter identity
                       (for [cell all-cells
                             n (range 2)]
                         (let [[x y] cell]
                           (when (or
                                  (= "grass" (level-map cell))
                                  (= "trees" (level-map cell)))
                             (sprite/make-sprite
                              (rand-nth [:grass-1 :grass-2 :grass-3])
                              :scale scale-2
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
                                (rand-nth [:tree-1 :tree-2 :tree-3 :tree-4 :tree-5 :tree-6])
                                :scale scale-2
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
                                 (rand-nth [:sand-1 :sand-2 :sand-3 :sand-4])
                                 :scale scale-2
                                 :x (math/rand-between -3000 3000)
                                 :y (math/rand-between -3000 3000)
                                 :xhandle 0.5 :yhandle 1.0
                                 :alpha 1.0
                                 )))))]
            (m/with-sprite canvas :world
              [caravan (sprite/make-sprite
                        :caravan
                        :scale (* 1.5 scale)
                        :xhandle 0.5 :yhandle 0.85
                        :alpha 1.0)

               walker (sprite/make-sprite
                       :char-1
                       :scale scale-2
                       :xhandle 0.5 :yhandle 1.0
                       :alpha 1.0
                       )]

              (let [bg-chan (chan)
                    click-chan (chan)
                    dest (atom (vec2/vec2 0.1 0.1))]

                ;; caravan button controller
                (c/button-thread caravan click-chan)

                ;; 'thread' to handle buttons on walker
                (w/button-thread walker click-chan)

                (set! (.-interactive (-> canvas :stage)) true)
                (set! (.-mousedown (-> canvas :stage)) (fn [ev] ;(log "MD" ev)
                                                         ))

                (set! (.-interactive (-> canvas :layer :bg)) true)
                (set! (.-mousedown (-> canvas :layer :bg))
                      (fn [ev]          ;(log "BG" ev)
                                        ;(log (.-originalEvent.clientX ev) (.-originalEvent.clientY ev))

                        ;; click animation
                        (go
                          (sound/play-sound :select-man 0.5 false)
                          (let [x (- (.-originalEvent.clientX ev)
                                     (/  (.-width (:canvas canvas)) 2))
                                y (- (.-originalEvent.clientY ev)
                                     (/ (.-height (:canvas canvas)) 2))]
                            (m/with-sprite canvas :ui
                              [click
                               (sprite/make-sprite
                                :click-mark-1
                                :scale scale-2
                                :x x :y y
                                :xhandle 0.45 :yhandle 0.6)]

                              (<! (events/wait-time 100)))
                            (m/with-sprite canvas :ui
                              [click
                               (sprite/make-sprite
                                :click-mark-2
                                :scale scale-2
                                :x x :y y
                                :xhandle 0.45 :yhandle 0.6)]

                              (<! (events/wait-time 100)))))



                        (swap! g/game #(-> %
                                           (assoc-in [:walker :action] :walk)
                                           (assoc-in [:walker :action-count] 0)
                                           (assoc-in [:walker :buttons] false)
                                           (assoc-in [:caravan :buttons] false)
                                           (assoc-in [:walker :chopping] nil)))
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

                ;; global handler maybe?
                (set! (.-onmousedown (:canvas canvas))
                      (fn [e] (when (= 2 (.-button e)) ;(log "right")
                                )))

                ;; no popup browser menu on right click
                (set! (.-oncontextmenu (:canvas canvas))
                      (fn [e] (.preventDefault e)))

                (.sort (.-children (-> canvas :layer :world)) depth-compare )


                ;; start ui control thread
                (ui-control
                 (fn [x y]
                   (let [xp x
                         yp y]
                     (sprite/set-pivot! (-> canvas :layer :world) xp yp)
                     (sprite/set-pivot! (-> canvas :layer :bg) xp yp)
                     (sprite/set-pivot! (-> canvas :layer :float) xp yp))
                   )
                 )



                (loop [n 0
                       b {:mass 5.0
                          :pos (vec2/vec2 0 0)
                          :vel (vec2/vec2 0 0)
                          :max-force 2.0
                          :max-speed 2.0}]


                  (.sort (.-children (-> canvas :layer :world)) depth-compare )
                  (swap! g/game #(-> %
                                     (update-in [:walker :action-count] inc)
                                     (update :frame inc)
                                     (update :plants update-plants)
                                     (update :flies update-flies)))

                  (<! (events/next-frame))

                  ;; planted
                  (let [{{:keys [action action-count]} :walker} @g/game]
                    (when (and (= :plant action)
                               (= (:plant-length @g/game) action-count))
                                        ;(log "PLANT")

                      ;;
                      ;; plant a plant?
                      ;;
                      (let [seeds (:seeds @g/game)]
                                        ;(log "seeds:" seeds)
                        (if (zero? seeds)
                          ;; deny
                          (do (sound/play-sound :tree-not-planted 0.5 false)
                              (swap! g/game
                                     #(-> %
                                          (assoc-in [:walker :action] :walk)
                                          (assoc-in [:walker :action-count] 0))))

                          (let [newplant (assoc (new-plant (:pos b))
                                                :growth-rate (:growth-rate @g/game))]
                            (sound/play-sound :tree-planted 0.5 false)
                            (.addChild (-> canvas :layer :world) (:sprite newplant))


                            (reset! dest (vec2/add
                                          (:pos b)
                                          (vec2/scale (vec2/random-unit) 40)))
                            (swap! g/game
                                   #(-> %
                                        (assoc-in [:walker :action] :walk)
                                        (assoc-in [:walker :action-count] 0)
                                        (update :plants conj newplant)
                                        (update :seeds dec)))

                            (set-seed-text (:seeds @g/game) (:dollars @g/game)))))))

                                        ;(log (str b))

                  (let [nb (b/arrive b @dest 30.0)]
                                        ;(log (str nb))
                    (sprite/set-pos! walker (:pos nb))
                    (if (= :plant (-> @g/game :walker :action))
                      (sprite/set-texture! walker
                                           (if (< (mod n 60) 30) :char-work-1 :char-work-2))
                      (sprite/set-texture! walker :char-1))


                    (recur
                     (inc n)
                     ;; this slight offset prevents the arrive silent crash I dont understand (pos goes to crazy values? divide by zero?
                     (assoc nb :pos (vec2/add (vec2/vec2 0.2 0.2) (:pos nb))))))))))))

    ))

(defonce _main (main))

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
