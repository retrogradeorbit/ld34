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
  (m/with-canvas canvas
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
            assets (into {}
                         (for [[key {:keys [pos size]}] a/assets]
                           [key
                            (texture/sub-texture sheet pos size)
                            ]))
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

            _ (log "!!!!")

            sfx-tree-planted (<! (sound/load-sound "/sfx/tree-planted.ogg"))
            sfx-tree-not-planted (<! (sound/load-sound "/sfx/tree-not-planted.ogg"))
            sfx-tree-chop (<! (sound/load-sound "/sfx/tree-chop.ogg"))
            sfx-select-man (<! (sound/load-sound "/sfx/select-man.ogg"))
            sfx-button-open (<! (sound/load-sound "/sfx/button-open.ogg"))
            sfx-button-close (<! (sound/load-sound "/sfx/button-close.ogg"))
            sfx-button-select (<! (sound/load-sound "/sfx/button-select.ogg"))
            sfx-tree-harvest (<! (sound/load-sound "/sfx/tree-harvest.ogg"))
            sfx-bug-spray (<! (sound/load-sound "/sfx/bug-spray.ogg"))
            sfx-tree-grow (<! (sound/load-sound "/sfx/tree-grow.ogg"))
            sfx-tree-hurt (<! (sound/load-sound "/sfx/tree-hurt.ogg"))
            sfx-fly-attack (<! (sound/load-sound "/sfx/fly-attack.ogg"))
            sfx-fly-change (<! (sound/load-sound "/sfx/fly-change.ogg"))

            _ (log "sfx loaded")

            new-plant
            (fn
              [pos]
              {
               :age 0
               :growth-rate 0.001         ;0.002         ; 0.001
               :id (keyword (gensym))
               :yield 1000
               :pos pos
               :sprite (sprite/make-sprite (:plant-1 assets)
                                           :x (vec2/get-x pos)
                                           :y (vec2/get-y pos)
                                           :scale scale-2
                                           :xhandle 0.5
                                           :yhandle 1.0)})

            new-flies (partial flies/make assets)

            new-hippy
            (fn [pos]
              {:pos pos
               :sprite (sprite/make-sprite (:hippy-left assets)
                                           :x (vec2/get-x pos)
                                           :y (vec2/get-y pos)
                                           :scale scale-2
                                           :xhandle 0.5
                                           :yhandle 1.0)})

            _ (g/init set-seed-text new-flies)

            update-seeds
            (fn []
              (let [state (-> @g/game :seeds)]
                nil
                ))

            update-flies (partial flies/update g/game)
            update-tree-chop (partial tree/update-chop g/game)
            chop-tree-go-thread (partial tree/chop-tree-go-thread g/game canvas)
            hippy-go-thread (partial hippy/hippy-go-thread g/game)

            ;; TODO: exit this thread on death
            fly-go-thread (partial flies/go-thread g/game)

            update-plants
            (fn [plants]
                                        ;(log "Update-plants:" (str plants))
              (doall (for [plant plants]
                       (sprite/set-texture! (:sprite plant)
                                            (nth
                                             [(:plant-1 assets)
                                              (:plant-2 assets)
                                              (:plant-3 assets)
                                              (:plant-4 assets)
                                              (:plant-5 assets)
                                              (:plant-6 assets)
                                              (:plant-7 assets)]
                                             (max 0 (min 6 (int (:age plant))))))))
              (into #{}
                    (map #(-> %
                              (update :age
                                      (fn [x]
                                        (let [next (+ x (-> % :growth-rate))]
                                          (when (> (int next) (int x))
                                            (sound/play-sound sfx-tree-grow 0.3 false))
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
             (hippy-go-thread g/game canvas assets hippy))))



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
                                ((rand-nth [:grass-1 :grass-2 :grass-3]) assets)
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
                                  ((rand-nth [:tree-1 :tree-2 :tree-3 :tree-4 :tree-5 :tree-6]) assets)
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
                                   ((rand-nth [:sand-1 :sand-2 :sand-3 :sand-4]) assets)
                                   :scale scale-2
                                   :x (math/rand-between -3000 3000)
                                   :y (math/rand-between -3000 3000)
                                   :xhandle 0.5 :yhandle 1.0
                                   :alpha 1.0
                                   )))))]
              (m/with-sprite canvas :world
                [caravan (sprite/make-sprite
                          (:caravan assets)
                          :scale (* 1.5 scale)
                          :xhandle 0.5 :yhandle 0.85
                          :alpha 1.0)

                 walker (sprite/make-sprite
                         (:char-1 assets)
                         :scale scale-2
                         :xhandle 0.5 :yhandle 1.0
                         :alpha 1.0
                         )]

                (let [bg-chan (chan)
                      click-chan (chan)
                      dest (atom (vec2/vec2 0.1 0.1))]

                  ;; caravan button controller
                  (c/button-thread canvas caravan assets click-chan)

                  ;; 'thread' to handle buttons on walker
                  (w/button-thread canvas walker assets click-chan)

                  ;; (go
                  ;;   ;; forever
                  ;;   (loop []
                  ;;     (<! (events/next-frame))

                  ;;     ;; buttons flips to true
                  ;;     (when (-> @g/game :walker :buttons)
                  ;;       (swap! g/game assoc-in [:caravan :buttons] false)
                  ;;       (sound/play-sound sfx-button-open 0.5 false)
                  ;;       ;; appear
                  ;;       (m/with-sprite canvas :float
                  ;;         [button (sprite/make-sprite
                  ;;                  (:button-grow assets)
                  ;;                  :scale scale-2
                  ;;                  :interactive true
                  ;;                  :mousedown #(put! click-chan :plant))
                  ;;          button-chop (sprite/make-sprite
                  ;;                       (:button-chop assets)
                  ;;                       :scale scale-2
                  ;;                       :interactive true
                  ;;                       :mousedown #(put! click-chan :chop))
                  ;;          button-spray (sprite/make-sprite
                  ;;                        (:button-spray assets)
                  ;;                        :scale scale-2
                  ;;                        :interactive true
                  ;;                        :mousedown #(put! click-chan :spray))]

                  ;;         (if (zero? (-> @g/game :seeds))
                  ;;           (sprite/set-alpha! button 0.5)
                  ;;           (sprite/set-alpha! button 1))

                  ;;         (if (< (:dollars @g/game) 100)
                  ;;           (sprite/set-alpha! button-spray 0.5)
                  ;;           (sprite/set-alpha! button-spray 1.0))

                  ;;         (let [closest-plant (first
                  ;;                              (filter
                  ;;                               #(> (:age %) 6)
                  ;;                               (sort-by
                  ;;                                #(vec2/distance-squared
                  ;;                                  (:pos %)
                  ;;                                  (sprite/get-pos walker))
                  ;;                                (:plants @g/game))))]
                  ;;           (if closest-plant
                  ;;             (let [distance (vec2/distance
                  ;;                             (:pos closest-plant)
                  ;;                             (sprite/get-pos walker))]

                  ;;               (if (< distance 15)
                  ;;                 (sprite/set-alpha! button-chop 1.0)
                  ;;                 (sprite/set-alpha! button-chop 0.5)))
                  ;;             (sprite/set-alpha! button-chop 0.5)
                  ;;             ))

                  ;;         ;; move button out and up
                  ;;         (loop [b {:mass 1 :pos
                  ;;                   (vec2/sub
                  ;;                    (sprite/get-pos walker)
                  ;;                    (vec2/vec2 0 50))
                  ;;                   :vel (vec2/vec2 0 -10)
                  ;;                   :max-force 1
                  ;;                   :max-speed 10}
                  ;;                b2 {:mass 1 :pos
                  ;;                    (vec2/sub
                  ;;                     (sprite/get-pos walker)
                  ;;                     (vec2/vec2 0 0))
                  ;;                    :vel
                  ;;                    (vec2/rotate
                  ;;                     (vec2/vec2 0 -10)
                  ;;                     (/ Math/PI 0.5 3))
                  ;;                    :max-force 1
                  ;;                    :max-speed 10}
                  ;;                b3 {:mass 1 :pos
                  ;;                    (sprite/get-pos walker)
                  ;;                    :vel (vec2/rotate
                  ;;                          (vec2/vec2 0 -10)
                  ;;                          (/ Math/PI 0.25 3))
                  ;;                    :max-force 1
                  ;;                    :max-speed 10}
                  ;;                ]
                  ;;           (sprite/set-pos! button (:pos b))
                  ;;           (sprite/set-pos! button-chop (:pos b2))
                  ;;           (sprite/set-pos! button-spray (:pos b3))
                  ;;                         ;(<! (events/next-frame))

                  ;;           ;; keep 'arriving' while buttons is still true
                  ;;           (if (-> @g/game :walker :buttons)
                  ;;             ;; buttons is still true
                  ;;             (let [[v c] (alts! #js [(events/next-frame) click-chan])]
                  ;;               (if (= c click-chan)
                  ;;                 (case v
                  ;;                   :plant
                  ;;                   ;; a button was clicked! exit and reset
                  ;;                   (if (zero? (-> @g/game :seeds))
                  ;;                     ;; turn off button

                  ;;                     (do
                  ;;                       (swap! g/game update-in [:walker :buttons] not)
                  ;;                       (sound/play-sound sfx-button-close 0.5 false)
                  ;;                       (loop [b b
                  ;;                              b2 b2
                  ;;                              b3 b3
                  ;;                              n 10]
                  ;;                         (sprite/set-pos! button (:pos b))
                  ;;                         (sprite/set-pos! button-chop (:pos b2))
                  ;;                         (sprite/set-pos! button-spray (:pos b3))
                  ;;                         (<! (events/next-frame))

                  ;;                         ;; return home
                  ;;                         (when (pos? n)
                  ;;                           (recur (b/arrive b
                  ;;                                            (sprite/get-pos walker)
                  ;;                                            50.0)
                  ;;                                  (b/arrive b2
                  ;;                                            (sprite/get-pos walker)
                  ;;                                            50.0)
                  ;;                                  (b/arrive b3
                  ;;                                            (sprite/get-pos walker)
                  ;;                                            50.0)
                  ;;                                  (dec n)))
                  ;;                         )
                  ;;                       )

                  ;;                     ;; select "plant seed"
                  ;;                     (do (sound/play-sound sfx-button-select 0.7 false)

                  ;;                         ;; grow and fade
                  ;;                         (loop [n 10]
                  ;;                           (when (pos? n)
                  ;;                             (effects/scale! button 1.05)
                  ;;                             (effects/scale-alpha! button 0.92)
                  ;;                             (<! (events/next-frame))
                  ;;                             (recur (dec n))))

                  ;;                         ;; user clicked PLANT
                  ;;                         (swap! g/game
                  ;;                                #(->
                  ;;                                  %
                  ;;                                  (update-in [:walker :buttons] not)
                  ;;                                  (assoc-in [:walker :action] :plant)
                  ;;                                  (assoc-in [:walker :action-count] 0)))))

                  ;;                   ;; select "sprayed"
                  ;;                   :spray
                  ;;                   (if (< (:dollars @g/game) 100)
                  ;;                     ;; exit
                  ;;                     (do
                  ;;                       (swap! g/game update-in [:walker :buttons] not)
                  ;;                       (sound/play-sound sfx-button-close 0.5 false)
                  ;;                       (loop [b b
                  ;;                              b2 b2
                  ;;                              b3 b3
                  ;;                              n 10]
                  ;;                         (sprite/set-pos! button (:pos b))
                  ;;                         (sprite/set-pos! button-chop (:pos b2))
                  ;;                         (sprite/set-pos! button-spray (:pos b3))
                  ;;                         (<! (events/next-frame))

                  ;;                         ;; return home
                  ;;                         (when (pos? n)
                  ;;                           (recur (b/arrive b
                  ;;                                            (sprite/get-pos walker)
                  ;;                                            50.0)
                  ;;                                  (b/arrive b2
                  ;;                                            (sprite/get-pos walker)
                  ;;                                            50.0)
                  ;;                                  (b/arrive b3
                  ;;                                            (sprite/get-pos walker)
                  ;;                                            50.0)
                  ;;                                  (dec n)))
                  ;;                         )
                  ;;                       )

                  ;;                     ;; spray
                  ;;                     (do




                  ;;                       ;; the spray
                  ;;                       (go
                  ;;                         (sound/play-sound sfx-bug-spray 0.3 false)
                  ;;                         (m/with-sprite canvas :float
                  ;;                           [spray (sprite/make-sprite
                  ;;                                   (:spray-1 assets)
                  ;;                                   :scale scale-2
                  ;;                                   :x 0
                  ;;                                   :y 0)]

                  ;;                           (sprite/set-pos! spray
                  ;;                                            (vec2/vec2
                  ;;                                             (.-position.x walker)
                  ;;                                             (- (.-position.y walker)
                  ;;                                                25)))

                  ;;                           ;; find all flies in collision with the spray
                  ;;                         ;(log "FLIES!!!")
                  ;;                           (let [flies (:flies @g/game)
                  ;;                                 touching (filter
                  ;;                                           #(< (vec2/distance-squared
                  ;;                                                (sprite/get-pos (:sprite %))
                  ;;                                                (sprite/get-pos spray))
                  ;;                                               ;; spray effective distance
                  ;;                                               (* 40 40))
                  ;;                                           flies)]
                  ;;                             (when (not (empty? touching))

                  ;;                         ;(log "REMOVING:" (count touching))



                  ;;                               ;; kill all the touching flies
                  ;;                               (doall (for [fly touching]
                  ;;                                        (.removeChild (-> canvas :layer :world)
                  ;;                                                      (:sprite fly))))

                  ;;                               (swap! g/game update :flies (fn [flies]
                  ;;                                                           (reduce disj
                  ;;                                                                   flies
                  ;;                                                                   touching)))

                  ;;                               ;; GAME LEVEL PLAY
                  ;;                               (log "NOW:" (count (:flies @g/game)))
                  ;;                               (when (zero? (count (:flies @g/game)))
                  ;;                                 (swap! g/game update :flies
                  ;;                                        (fn [flies]
                  ;;                                          (let [num-new-flies (:level @g/game)
                  ;;                                                flies-to-add
                  ;;                                                (map #(new-flies (vec2/scale (vec2/random-unit) 2500))
                  ;;                                                     (range num-new-flies))]
                  ;;                                            (doall
                  ;;                                             (for [fly flies-to-add]
                  ;;                                               (do
                  ;;                                                 (.addChild (-> canvas :layer :world)
                  ;;                                                            (:sprite fly))
                  ;;                                                 (fly-go-thread fly))))
                  ;;                                            (into flies flies-to-add)
                  ;;                                            )))
                  ;;                                 )
                  ;;                               (let [hippies-should-be (dec (:level @g/game))
                  ;;                                     hippies-are (count (:hippies @g/game))]
                  ;;                                 (when (< hippies-are
                  ;;                                          hippies-should-be)
                  ;;                                   (let [to-add (- hippies-should-be hippies-are)]
                  ;;                                     (swap! g/game update :hippies
                  ;;                                            (fn [hippies]
                  ;;                                              (let [
                  ;;                                                    hippies-to-add
                  ;;                                                    (map #(new-hippy (vec2/scale (vec2/random-unit) 2500))
                  ;;                                                         (range to-add))]
                  ;;                                                (doall
                  ;;                                                 (for [hippy hippies-to-add]
                  ;;                                                   (do
                  ;;                                                     (.addChild (-> canvas :layer :world)
                  ;;                                                                (:sprite hippy))
                  ;;                                                     (hippy-go-thread hippy))))
                  ;;                                                (into hippies hippies-to-add)))))))))

                  ;;                           (let [spray-frames
                  ;;                                 [:spray-1
                  ;;                                  :spray-2
                  ;;                                  :spray-3
                  ;;                                  :spray-4
                  ;;                                  :spray-5]]
                  ;;                             (loop [f 0]
                  ;;                               (sprite/set-texture!
                  ;;                                spray
                  ;;                                (
                  ;;                                 (nth spray-frames f)
                  ;;                                 assets))
                  ;;                               (<! (events/wait-time 100))
                  ;;                               (when (< f (dec (count spray-frames)))
                  ;;                                 (recur (inc f)))))))

                  ;;                       ;; grow and fade
                  ;;                       (loop [n 10]
                  ;;                         (when (pos? n)
                  ;;                           (effects/scale! button-spray 1.05)
                  ;;                           (effects/scale-alpha! button-spray 0.92)
                  ;;                           (<! (events/next-frame))
                  ;;                           (recur (dec n))))

                  ;;                       (swap! g/game
                  ;;                              #(->
                  ;;                                %
                  ;;                                (update-in [:walker :buttons] not)
                  ;;                                (assoc-in [:walker :action] :walk)
                  ;;                                (assoc-in [:walker :chopping] nil)
                  ;;                                (update :dollars - 100)
                  ;;                                (assoc-in [:walker :action-count] 0)))

                  ;;                       (set-seed-text (:seeds @g/game) (:dollars @g/game))))

                  ;;                   ;; select "chopped"
                  ;;                   :chop
                  ;;                   (if (= 1 (.-alpha button-chop))

                  ;;                     ;; chop
                  ;;                     (do
                  ;;                       (sound/play-sound sfx-button-select 0.7 false)

                  ;;                       ;; grow and fade
                  ;;                       (loop [n 10]
                  ;;                         (when (pos? n)
                  ;;                           (effects/scale! button-chop 1.05)
                  ;;                           (effects/scale-alpha! button-chop 0.92)
                  ;;                           (<! (events/next-frame))
                  ;;                           (recur (dec n))))

                  ;;                       (let [closest (first
                  ;;                                      (filter
                  ;;                                       (fn [pl] (> (:age pl) 6))
                  ;;                                       (sort-by
                  ;;                                        (fn [pl] (vec2/distance-squared
                  ;;                                                  (:pos pl)
                  ;;                                                  (sprite/get-pos walker)))
                  ;;                                        (:plants @g/game))))]

                  ;;                         ;; user clicked chop
                  ;;                         (chop-tree-go-thread closest set-seed-text)

                  ;;                         (swap! g/game
                  ;;                                #(->
                  ;;                                  %
                  ;;                                  (update-in [:walker :buttons] not)
                  ;;                                  (assoc-in [:walker :action] :chop)
                  ;;                                  (assoc-in [:walker :chopping]
                  ;;                                            closest
                  ;;                                            )
                  ;;                                  (assoc-in [:walker :action-count] 0))))





                  ;;                       )

                  ;;                     (do
                  ;;                       (swap! g/game update-in [:walker :buttons] not)
                  ;;                       (sound/play-sound sfx-button-close 0.5 false)
                  ;;                       (loop [b b
                  ;;                              b2 b2
                  ;;                              b3 b3
                  ;;                              n 10]
                  ;;                         (sprite/set-pos! button (:pos b))
                  ;;                         (sprite/set-pos! button-chop (:pos b2))
                  ;;                         (sprite/set-pos! button-spray (:pos b3))
                  ;;                         (<! (events/next-frame))

                  ;;                         ;; return home
                  ;;                         (when (pos? n)
                  ;;                           (recur (b/arrive b
                  ;;                                            (sprite/get-pos walker)
                  ;;                                            50.0)
                  ;;                                  (b/arrive b2
                  ;;                                            (sprite/get-pos walker)
                  ;;                                            50.0)
                  ;;                                  (b/arrive b3
                  ;;                                            (sprite/get-pos walker)
                  ;;                                            50.0)
                  ;;                                  (dec n)))
                  ;;                         )
                  ;;                       )

                  ;;                     )


                  ;;                   ;; unknown button clicked
                  ;;                   (do
                  ;;                     (swap! g/game update-in [:walker :buttons] not)
                  ;;                     (sound/play-sound sfx-button-close 0.5 false)
                  ;;                     (loop [b b
                  ;;                            b2 b2
                  ;;                            b3 b3
                  ;;                            n 10]
                  ;;                       (sprite/set-pos! button (:pos b))
                  ;;                       (sprite/set-pos! button-chop (:pos b2))
                  ;;                       (sprite/set-pos! button-spray (:pos b3))
                  ;;                       (<! (events/next-frame))

                  ;;                       ;; return home
                  ;;                       (when (pos? n)
                  ;;                         (recur (b/arrive b
                  ;;                                          (sprite/get-pos walker)
                  ;;                                          50.0)
                  ;;                                (b/arrive b2
                  ;;                                          (sprite/get-pos walker)
                  ;;                                          50.0)
                  ;;                                (b/arrive b3
                  ;;                                          (sprite/get-pos walker)
                  ;;                                          50.0)
                  ;;                                (dec n)))
                  ;;                       )
                  ;;                     ))

                  ;;                 ;; no button is clicked.
                  ;;                 (recur (b/arrive b
                  ;;                                  (vec2/sub
                  ;;                                   (sprite/get-pos walker)
                  ;;                                   (vec2/vec2 0 100)) 50.0)
                  ;;                        (b/arrive b2
                  ;;                                  (vec2/sub
                  ;;                                   (sprite/get-pos walker)
                  ;;                                   (vec2/rotate
                  ;;                                    (vec2/vec2 0 70)
                  ;;                                    (/ Math/PI 0.5 3))) 50.0)
                  ;;                        (b/arrive b3
                  ;;                                  (vec2/sub
                  ;;                                   (sprite/get-pos walker)
                  ;;                                   (vec2/rotate
                  ;;                                    (vec2/vec2 0 70)
                  ;;                                    (/ Math/PI 0.25 3))) 50.0)

                  ;;                        )))

                  ;;             ;; buttons is no longer true
                  ;;             (do (sound/play-sound sfx-button-close 0.5 false)
                  ;;                 (loop [b b
                  ;;                        b2 b2
                  ;;                        b3 b3
                  ;;                        n 10]
                  ;;                   (sprite/set-pos! button (:pos b))
                  ;;                   (sprite/set-pos! button-chop (:pos b2))
                  ;;                   (sprite/set-pos! button-spray (:pos b3))
                  ;;                   (<! (events/next-frame))

                  ;;                   ;; return home
                  ;;                   (when (pos? n)
                  ;;                     (recur (b/arrive b
                  ;;                                      (sprite/get-pos walker)
                  ;;                                      50.0)
                  ;;                            (b/arrive b2
                  ;;                                      (sprite/get-pos walker)
                  ;;                                      50.0)
                  ;;                            (b/arrive b3
                  ;;                                      (sprite/get-pos walker)
                  ;;                                      50.0)
                  ;;                            (dec n)))
                  ;;                   )))
                  ;;           )))

                  ;;     (recur)))

                  (set! (.-interactive walker) true)
                  (set! (.-mousedown walker)
                        ;; click on the walker
                        (fn [ev]          ;(log "mousedown" ev)


                          ;; open action icons go-thread

                          (swap! g/game update-in [:walker :buttons] not)


                          ))
                  (set! (.-interactive (-> canvas :stage)) true)
                  (set! (.-mousedown (-> canvas :stage)) (fn [ev] ;(log "MD" ev)
                                                           ))

                  (set! (.-interactive (-> canvas :layer :bg)) true)
                  (set! (.-mousedown (-> canvas :layer :bg))
                        (fn [ev]          ;(log "BG" ev)
                                        ;(log (.-originalEvent.clientX ev) (.-originalEvent.clientY ev))

                          ;; click animation
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
                                  :scale scale-2
                                  :x x :y y
                                  :xhandle 0.45 :yhandle 0.6)]

                                (<! (events/wait-time 100)))
                              (m/with-sprite canvas :ui
                                [click
                                 (sprite/make-sprite
                                  (:click-mark-2 assets)
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
                            (do (sound/play-sound sfx-tree-not-planted 0.5 false)
                                (swap! g/game
                                       #(-> %
                                            (assoc-in [:walker :action] :walk)
                                            (assoc-in [:walker :action-count] 0))))

                            (let [newplant (assoc (new-plant (:pos b))
                                                  :growth-rate (:growth-rate @g/game))]
                              (sound/play-sound sfx-tree-planted 0.5 false)
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
                                             ((if (< (mod n 60) 30)
                                                :char-work-1 :char-work-2) assets))
                        (sprite/set-texture! walker (:char-1 assets)))

                                        ;(update-tree-chop)

                      (recur
                       (inc n)
                       ;; this slight offset prevents the arrive silent crash I dont understand (pos goes to crazy values? divide by zero?
                       (assoc nb :pos (vec2/add (vec2/vec2 0.2 0.2) (:pos nb)))))))))))))))

(defonce _main (main))

(println "Edits to this text should show up in your developer console.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world!"}))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
