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
              [ld34.effects :as effects]
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
          "sfx/button-open.ogg"
          "sfx/button-close.ogg"
          "sfx/button-select.ogg"
          "sfx/tree-planted.ogg"
          "sfx/tree-not-planted.ogg"
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
          scale-2 [(* 2 scale) (* 2 scale)]

          set-pos-relative!
          (fn [spr corner space]
            (let [h-width (/ (.-width (:canvas canvas)) 2)
                  h-height (/ (.-height (:canvas canvas)) 2)]
              (case corner
                :top-left
                (do (sprite/set-anchor! spr 0 0)
                    (sprite/set-pos! spr (- space h-width) (- space h-height)))
                )
              )

            )

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

          sfx-select-man (<! (sound/load-sound "/sfx/select-man.ogg"))
          sfx-button-open (<! (sound/load-sound "/sfx/button-open.ogg"))
          sfx-button-close (<! (sound/load-sound "/sfx/button-close.ogg"))
          sfx-button-select (<! (sound/load-sound "/sfx/button-select.ogg"))
          sfx-tree-planted (<! (sound/load-sound "/sfx/tree-planted.ogg"))
          sfx-tree-not-planted (<! (sound/load-sound "/sfx/tree-not-planted.ogg"))




          new-plant
          (fn
            [pos]
            {
             :age 0
             :growth-rate 0.001
             :pos pos
             :sprite (sprite/make-sprite (:plant-1 assets)
                                         :x (vec2/get-x pos)
                                         :y (vec2/get-y pos)
                                         :scale scale-2
                                         :xhandle 0.5
                                         :yhandle 1.0)})

          new-flies
          (fn [pos]
            {:pos pos
             :sprite (sprite/make-sprite (:flies-1 assets)
                                         :x (vec2/get-x pos)
                                         :y (vec2/get-y pos)
                                         :scale scale-2
                                         :xhandle 0.5
                                         :yhandle 1.0)})

          game
          (atom
           {
            :frame 0
            :seeds 3
            :dollars 0
            :walker {:buttons false
                     :action :walk ;; :walk :plant
                     :action-count 0 ;; when we want to measure how long weve been in an action for
                     ;; if were harvesting, this is the plant
                     :chopping nil
                     }
            :plants #{}
            :flies #{(new-flies (vec2/vec2 0 2000))}
            })


          texture-cycle
          (fn [texture-list frame frame-length]
            (let [total-frames (count texture-list)]
              (texture-list
               (mod
                (int (/ frame frame-length))
                total-frames))))

          update-seeds
          (fn []
            (let [state (-> @game :seeds)]
              nil
              ))

          update-flies
          (fn
            [flies]
            (doall (for [fly flies]
                     (sprite/set-texture!
                      (:sprite fly)
                      (texture-cycle [(:flies-1 assets)
                                      (:flies-2 assets)
                                      (:flies-3 assets)
                                      (:flies-4 assets)]
                                     (:frame @game)
                                     5))))
            (into #{}
                  (map
                   ;; update fly
                   identity
                   #_ (fn [{:keys [sprite] :as fly}]

                        )
                   flies))
            )

          update-tree-chop
          (fn []
            (when (-> @game :walker :chopping)
              (let [frame (-> @game :walker :action-count)
                    plant (-> @game :walker :chopping)
                    x (.-position.x (:sprite plant))
                    y (.-position.y (:sprite plant))]
                (if (= 0 (mod (int (/ frame 5)) 2))
                  (sprite/set-pos! (:sprite plant) (+ x 1) y)
                  (sprite/set-pos! (:sprite plant) (- x 1) y)))))

          fly-go-thread
          (fn [{:keys [pos sprite] :as fly}]
            (go
              (sprite/set-pos! sprite pos)

              (loop []
                ;; wait for a random time before looking for target (lower cpu)
                (<! (events/wait-time (math/rand-between 1000 5000)))

                (let [closest-plant
                      (first
                       (filter
                        #(> (:age %) 2)
                        (sort-by
                         #(vec2/distance-squared
                           (:pos %)
                           (vec2/vec2 (.-position.x sprite)
                                      (.-position.y sprite)))
                         (:plants @game))))]
                  (log "close:" closest-plant)
                  (when closest-plant
                    (when (< (vec2/distance-squared
                              (vec2/vec2 (.-position.x sprite)
                                         (.-position.y sprite))
                              (:pos closest-plant))
                             4)
                      (swap! game update :plants
                             #(-> %
                                  (disj closest-plant)
                                  (conj (update closest-plant :age
                                                (fn [x] (- x 1)))))))

                    (loop [b {:mass 0.5
                              :pos (vec2/vec2 (.-position.x sprite)
                                              (.-position.y sprite))
                              :vel (vec2/zero)
                              :max-force 0.01
                              :max-speed 1}]
                      (sprite/set-pos! sprite (:pos b))
                      (<! (events/next-frame))
                      (when (> (vec2/distance-squared
                                (:pos b)
                                (:pos closest-plant))
                               4)
                        (recur
                         (b/arrive b (vec2/add
                                      (vec2/vec2 0 2)
                                      (:pos closest-plant))
                                   100)
                         )))
                    )
                  )

                #_ (loop [b {:mass 0.5
                             :pos pos
                             :vel (vec2/zero)
                             :max-force 0.01
                             :max-speed 1}]
                     (sprite/set-pos! sprite (:pos b))
                     (<! (events/next-frame))
                     (recur
                      (b/arrive b (vec2/vec2 500 500) 100)
                      ))

                (recur)
                ))
            )

          update-plants
          (fn [plants]
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
                            (update :age (fn [x] (+ x (-> % :growth-rate))))
                            )
                       plants))
            )


          ]

      ;; add the first fly
      (doall
       (for [fly (-> @game :flies)]
         (do
           (.addChild (-> canvas :layer :world) (:sprite fly))
           (fly-go-thread fly))))

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
                [walker (sprite/make-sprite
                         (:char-1 assets)
                         :scale scale-2
                         :x 0 :y 0
                         :xhandle 0.5 :yhandle 1.0
                         :alpha 1.0
                         )]

                (let [bg-chan (chan)
                      click-chan (chan)
                      dest (atom (vec2/vec2 0.1 0.1))]

                  ;; 'thread' to handle buttons on walker
                  (go
                    ;; forever
                    (loop []
                      (<! (events/next-frame))

                      ;; buttons flips to true
                      (when (-> @game :walker :buttons)
                        (sound/play-sound sfx-button-open 0.5 false)
                        ;; appear
                        (m/with-sprite canvas :float
                          [button (sprite/make-sprite
                                   (:button-grow assets)
                                   :scale scale-2
                                   :x 0
                                   :y 0)
                           button-chop (sprite/make-sprite
                                        (:button-chop assets)
                                        :scale scale-2
                                        :x 0
                                        :y 0)
                           button-spray (sprite/make-sprite
                                         (:button-spray assets)
                                         :scale scale-2
                                         :x 0
                                         :y 0)]
                          ;; on click handlers
                          (set! (.-interactive button) true)
                          (set! (.-mousedown button)
                                ;; click on the walker
                                (fn [ev] (log "button mousedown" ev)
                                  (put! click-chan :plant)
                                  ))

                          (set! (.-interactive button-chop) true)
                          (set! (.-mousedown button-chop)
                                ;; click on the walker
                                (fn [ev] (log "button mousedown" ev)
                                  (put! click-chan :chop)
                                  ))

                          (set! (.-interactive button-spray) true)
                          (set! (.-mousedown button-spray)
                                ;; click on the walker
                                (fn [ev] (log "button mousedown" ev)
                                  (put! click-chan :spray)
                                  ))



                          (if (zero? (-> @game :seeds))
                            (sprite/set-alpha! button 0.5)
                            (sprite/set-alpha! button 1))

                          (sprite/set-alpha! button-spray 0.5)

                          (let [closest-plant (first
                                               (filter
                                                #(> (:age %) 6)
                                                (sort-by
                                                 #(vec2/distance-squared
                                                   (:pos %)
                                                   (vec2/vec2 (.-position.x walker)
                                                              (.-position.y walker)))
                                                 (:plants @game))))]
                            (if closest-plant
                              (let [distance (vec2/distance
                                              (:pos closest-plant)
                                              (vec2/vec2 (.-position.x walker)
                                                         (.-position.y walker)))]

                                (if (< distance 15)
                                  (sprite/set-alpha! button-chop 1.0)
                                  (sprite/set-alpha! button-chop 0.5)))
                              (sprite/set-alpha! button-chop 0.5)
                              ))

                          ;; move button out and up
                          (loop [b {:mass 1 :pos
                                    (vec2/sub
                                     (vec2/vec2 (.-position.x walker)
                                                (.-position.y walker))
                                     (vec2/vec2 0 50))
                                    :vel (vec2/vec2 0 -10)
                                    :max-force 1
                                    :max-speed 10}
                                 b2 {:mass 1 :pos
                                     (vec2/sub
                                      (vec2/vec2 (.-position.x walker)
                                                 (.-position.y walker))
                                      (vec2/vec2 0 0))
                                     :vel
                                     (vec2/rotate
                                      (vec2/vec2 0 -10)
                                      (/ Math/PI 0.5 3))
                                     :max-force 1
                                     :max-speed 10}
                                 b3 {:mass 1 :pos
                                     (vec2/vec2 (.-position.x walker)
                                                (.-position.y walker))
                                     :vel (vec2/rotate
                                           (vec2/vec2 0 -10)
                                           (/ Math/PI 0.25 3))
                                     :max-force 1
                                     :max-speed 10}
                                 ]
                            (sprite/set-pos! button (:pos b))
                            (sprite/set-pos! button-chop (:pos b2))
                            (sprite/set-pos! button-spray (:pos b3))
                                        ;(<! (events/next-frame))

                            ;; keep 'arriving' while buttons is still true
                            (if (-> @game :walker :buttons)
                              ;; buttons is still true
                              (let [[v c] (alts! #js [(events/next-frame) click-chan])]
                                (if (= c click-chan)
                                  (case v
                                    :plant
                                    ;; a button was clicked! exit and reset
                                    (if (zero? (-> @game :seeds))
                                      ;; turn off button

                                      (do
                                        (swap! game update-in [:walker :buttons] not)
                                        (sound/play-sound sfx-button-close 0.5 false)
                                        (loop [b b
                                               b2 b2
                                               b3 b3
                                               n 10]
                                          (sprite/set-pos! button (:pos b))
                                          (sprite/set-pos! button-chop (:pos b2))
                                          (sprite/set-pos! button-spray (:pos b3))
                                          (<! (events/next-frame))

                                          ;; return home
                                          (when (pos? n)
                                            (recur (b/arrive b
                                                             (vec2/vec2 (.-position.x walker)
                                                                        (.-position.y walker))
                                                             50.0)
                                                   (b/arrive b2
                                                             (vec2/vec2 (.-position.x walker)
                                                                        (.-position.y walker))
                                                             50.0)
                                                   (b/arrive b3
                                                             (vec2/vec2 (.-position.x walker)
                                                                        (.-position.y walker))
                                                             50.0)
                                                   (dec n)))
                                          )
                                        )

                                      ;; select "plant seed"
                                      (do (sound/play-sound sfx-button-select 0.7 false)

                                          ;; grow and fade
                                          (loop [n 10]
                                            (when (pos? n)
                                              (effects/scale! button 1.05)
                                              (effects/scale-alpha! button 0.92)
                                              (<! (events/next-frame))
                                              (recur (dec n))))

                                          ;; user clicked PLANT
                                          (swap! game
                                                 #(->
                                                   %
                                                   (update-in [:walker :buttons] not)
                                                   (assoc-in [:walker :action] :plant)
                                                   (assoc-in [:walker :action-count] 0)))))

                                    ;; select "chopped"
                                    :chop
                                    (if (= 1 (.-alpha button-chop))

                                      ;; chop
                                      (do
                                        (sound/play-sound sfx-button-select 0.7 false)

                                        ;; grow and fade
                                        (loop [n 10]
                                          (when (pos? n)
                                            (effects/scale! button-chop 1.05)
                                            (effects/scale-alpha! button-chop 0.92)
                                            (<! (events/next-frame))
                                            (recur (dec n))))

                                        ;; user clicked chop
                                        (swap! game
                                               #(->
                                                 %
                                                 (update-in [:walker :buttons] not)
                                                 (assoc-in [:walker :action] :chop)
                                                 (assoc-in [:walker :chopping]
                                                           (first
                                                            (filter
                                                             (fn [pl] (> (:age pl) 6))
                                                             (sort-by
                                                              (fn [pl] (vec2/distance-squared
                                                                         (:pos pl)
                                                                         (vec2/vec2 (.-position.x walker)
                                                                                    (.-position.y walker))))
                                                              (:plants @game))))
                                                           )
                                                 (assoc-in [:walker :action-count] 0)))





                                        )

                                      (do
                                        (swap! game update-in [:walker :buttons] not)
                                        (sound/play-sound sfx-button-close 0.5 false)
                                        (loop [b b
                                               b2 b2
                                               b3 b3
                                               n 10]
                                          (sprite/set-pos! button (:pos b))
                                          (sprite/set-pos! button-chop (:pos b2))
                                          (sprite/set-pos! button-spray (:pos b3))
                                          (<! (events/next-frame))

                                          ;; return home
                                          (when (pos? n)
                                            (recur (b/arrive b
                                                             (vec2/vec2 (.-position.x walker)
                                                                        (.-position.y walker))
                                                             50.0)
                                                   (b/arrive b2
                                                             (vec2/vec2 (.-position.x walker)
                                                                        (.-position.y walker))
                                                             50.0)
                                                   (b/arrive b3
                                                             (vec2/vec2 (.-position.x walker)
                                                                        (.-position.y walker))
                                                             50.0)
                                                   (dec n)))
                                          )
                                        )

                                      )


                                    ;; unknown button clicked
                                    (do
                                      (swap! game update-in [:walker :buttons] not)
                                      (sound/play-sound sfx-button-close 0.5 false)
                                      (loop [b b
                                             b2 b2
                                             b3 b3
                                             n 10]
                                        (sprite/set-pos! button (:pos b))
                                        (sprite/set-pos! button-chop (:pos b2))
                                        (sprite/set-pos! button-spray (:pos b3))
                                        (<! (events/next-frame))

                                        ;; return home
                                        (when (pos? n)
                                          (recur (b/arrive b
                                                           (vec2/vec2 (.-position.x walker)
                                                                      (.-position.y walker))
                                                           50.0)
                                                 (b/arrive b2
                                                           (vec2/vec2 (.-position.x walker)
                                                                      (.-position.y walker))
                                                           50.0)
                                                 (b/arrive b3
                                                           (vec2/vec2 (.-position.x walker)
                                                                      (.-position.y walker))
                                                           50.0)
                                                 (dec n)))
                                        )
                                      )
                                    )

                                  ;; no button is clicked.
                                  (recur (b/arrive b
                                                   (vec2/sub
                                                    (vec2/vec2 (.-position.x walker)
                                                               (.-position.y walker))
                                                    (vec2/vec2 0 100)) 50.0)
                                         (b/arrive b2
                                                   (vec2/sub
                                                    (vec2/vec2 (.-position.x walker)
                                                               (- (.-position.y walker) 0))
                                                    (vec2/rotate
                                                     (vec2/vec2 0 70)
                                                     (/ Math/PI 0.5 3))) 50.0)
                                         (b/arrive b3
                                                   (vec2/sub
                                                    (vec2/vec2 (.-position.x walker)
                                                               (- (.-position.y walker) 0))
                                                    (vec2/rotate
                                                     (vec2/vec2 0 70)
                                                     (/ Math/PI 0.25 3))) 50.0)

                                         )))

                              ;; buttons is no longer true
                              (do (sound/play-sound sfx-button-close 0.5 false)
                                  (loop [b b
                                         b2 b2
                                         b3 b3
                                         n 10]
                                    (sprite/set-pos! button (:pos b))
                                    (sprite/set-pos! button-chop (:pos b2))
                                    (sprite/set-pos! button-spray (:pos b3))
                                    (<! (events/next-frame))

                                    ;; return home
                                    (when (pos? n)
                                      (recur (b/arrive b
                                                       (vec2/vec2 (.-position.x walker)
                                                                  (.-position.y walker))
                                                       50.0)
                                             (b/arrive b2
                                                       (vec2/vec2 (.-position.x walker)
                                                                  (.-position.y walker))
                                                       50.0)
                                             (b/arrive b3
                                                       (vec2/vec2 (.-position.x walker)
                                                                  (.-position.y walker))
                                                       50.0)
                                             (dec n)))
                                    )))
                            )))

                      (recur)))

                  (set! (.-interactive walker) true)
                  (set! (.-mousedown walker)
                        ;; click on the walker
                        (fn [ev] (log "mousedown" ev)


                          ;; open action icons go-thread

                          (swap! game update-in [:walker :buttons] not)


                          ))
                  (set! (.-interactive (-> canvas :stage)) true)
                  (set! (.-mousedown (-> canvas :stage)) (fn [ev] (log "MD" ev)))

                  (set! (.-interactive (-> canvas :layer :bg)) true)
                  (set! (.-mousedown (-> canvas :layer :bg))
                        (fn [ev] (log "BG" ev)
                          (log (.-originalEvent.clientX ev) (.-originalEvent.clientY ev))

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



                          (swap! game #(-> %
                                           (assoc-in [:walker :action] :walk)
                                           (assoc-in [:walker :action-count] 0)
                                           (assoc-in [:walker :buttons] false)
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
                        (fn [e] (when (= 2 (.-button e)) (log "right"))))

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
                    (swap! game #(-> %
                                     (update-in [:walker :action-count] inc)
                                     (update :frame inc)
                                     (update :plants update-plants)
                                     (update :flies update-flies)))

                    (<! (events/next-frame))

                    ;; planted
                    (let [{{:keys [action action-count]} :walker} @game]
                      (when (and (= :plant action)
                                 (= 400 action-count))
                        (log "PLANT")

                        ;;
                        ;; plant a plant?
                        ;;
                        (let [seeds (:seeds @game)]
                          (log "seeds:" seeds)
                          (if (zero? seeds)
                            ;; deny
                            (do (sound/play-sound sfx-tree-not-planted 0.5 false)
                                (swap! game
                                       #(-> %
                                            (assoc-in [:walker :action] :walk)
                                            (assoc-in [:walker :action-count] 0))))

                            (let [newplant (new-plant (:pos b))]
                              (sound/play-sound sfx-tree-planted 0.5 false)
                              (.addChild (-> canvas :layer :world) (:sprite newplant))


                              (reset! dest (vec2/add
                                            (:pos b)
                                            (vec2/scale (vec2/random-unit) 40)))
                              (swap! game
                                     #(-> %
                                          (assoc-in [:walker :action] :walk)
                                          (assoc-in [:walker :action-count] 0)
                                          (update :plants conj newplant)
                                          (update :seeds dec)))

                              (set-seed-text (:seeds @game) (:dollars @game))
                              )))))

                                        ;(log (str b))

                    (let [nb (b/arrive b @dest 30.0)]
                                        ;(log (str nb))
                      (sprite/set-pos! walker (:pos nb))
                      (if (= :plant (-> @game :walker :action))
                        (sprite/set-texture! walker
                                             ((if (< (mod n 60) 30)
                                                :char-work-1 :char-work-2) assets))
                        (sprite/set-texture! walker (:char-1 assets)))

                      (update-tree-chop)

                      (recur
                       (inc n)
                       ;; this slight offset prevents the arrive silent crash I dont understand (pos goes to crazy values? divide by zero?
                       (assoc nb :pos (vec2/add (vec2/vec2 0.2 0.2) (:pos nb))))
                      )

                    ))

                                        ;(<! (events/wait-time 300000))
                ))))

        )


      )

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
