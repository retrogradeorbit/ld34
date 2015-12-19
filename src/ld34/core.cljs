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

          new-flies
          (fn [pos]
            {:pos pos
             :sprite (sprite/make-sprite (:flies-1 assets)
                                         :x (vec2/get-x pos)
                                         :y (vec2/get-y pos)
                                         :scale scale-2
                                         :xhandle 0.5
                                         :yhandle 1.0)})

          new-hippy
          (fn [pos]
            {:pos pos
             :sprite (sprite/make-sprite (:hippy-left assets)
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
                     :level 0
                     }
            :plants #{}
            :flies #{(new-flies (vec2/scale (vec2/random-unit) 1000))}
            :hippies #{}
            :caravan {:buttons false}

            :max-seeds 1
            :growth-rate 0.002
            :chop-num 5
            :chop-length 60
            :plant-length 400
            :level 1
            :plant-multiplier 1

            :levels {
                     :man
                     {
                      :cost 1000
                      :activate (fn [g]
                                  (let [res
                                        (-> g
                                            (update :dollars - (-> g :levels :man :cost))
                                            (update-in [:levels :man :cost] * 2)
                                            (update :chop-num #(max 1 (dec %)))
                                            (update :chop-length #(max 10 (- % 10)))
                                            (update :plant-length #(max 30 (- % 70)))
                                            (update :level inc))]
                                    (set-seed-text (:seeds res) (:dollars res))

                                    res))
                      }

                     :faster
                     {
                      :cost 5000
                      :activate (fn [g]
                                  (let [res
                                        (-> g
                                            (update :dollars - (-> g :levels :faster :cost))
                                            (update-in [:levels :faster :cost] * 3)
                                            (update :growth-rate * 2)
                                            (update :level inc))]
                                    (set-seed-text (:seeds res) (:dollars res))
                                    res))
                      }

                     :seed
                     {
                      :cost 10000
                      :activate (fn [g]
                                  (let [res
                                        (-> g
                                            (update :dollars - (-> g :levels :seed :cost))
                                            (update-in [:levels :seed :cost] * 3)
                                            (update :max-seeds inc)
                                            (update :plant-multiplier * 2)
                                            (update :level inc))]
                                    (set-seed-text (:seeds res) (:dollars res))
                                    res))
                      }}

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
            (into #{} flies))

          ;; run each frame, animate the chopping tree
          update-tree-chop
          (fn []
            (when (-> @game :walker :chopping)
              (let [frame (-> @game :walker :action-count)
                    plant (-> @game :walker :chopping)
                    x (.-position.x (:sprite plant))
                    y (.-position.y (:sprite plant))
                    left-right (= 0 (mod (int (/ frame 5)) 2))
                    intensity (mod (/ frame 30) 4)]
                (if left-right
                  (sprite/set-pos! (:sprite plant) (+ x intensity) y)
                  (sprite/set-pos! (:sprite plant) (- x intensity) y)))))

          chop-tree-go-thread
          (fn [{:keys [pos sprite id yield] :as tree}]
            (go
                                        ;(log "CHOP_TREE" sprite pos tree)
              (loop [chop-num 0]

                (if (> chop-num (:chop-num @game))
                  ;; tree chopped down
                  (do
                    (sound/play-sound sfx-tree-harvest  0.5 false)
                    (.removeChild (-> canvas :layer :world) sprite)

                    (swap! game
                           #(-> %
                                (assoc-in [:walker :action] :walk)
                                (update :plants disj (first (filter
                                                             (fn [pl]
                                                               (= (:sprite pl) sprite))
                                                             (:plants @game))))
                                (update :dollars + (* yield (:plant-multiplier %)))
                                (update :seeds + (:max-seeds %))))
                    (set-seed-text (:seeds @game) (:dollars @game)))


                  (do

                    (sprite/set-pos! sprite pos)
                    (<! (events/wait-time 30))

                    (if (= :walk (-> @game :walker :action))
                      ;; exit
                      nil


                      (do

                        ;; shake
                        (sound/play-sound sfx-tree-chop 0.5 false)

                        (loop [f 0]
                          (when (< f (:chop-length @game))
                                        ;(log "shake" (effects/shake 10 2 0.07 f))
                            (sprite/set-pos! sprite (+
                                                     (vec2/get-x pos)
                                                     (effects/shake 10 2 0.07 f))
                                             (vec2/get-y pos))
                            (<! (events/next-frame))
                            (recur (inc f))
                            )

                          )
                        (recur (inc chop-num)))))))))

          hippy-go-thread
          (fn [{:keys [pos sprite] :as hippy}]
            (go
              (sprite/set-pos! sprite pos)

              (loop [boid {:mass 5
                           :pos (sprite/get-pos sprite)
                           :vel (vec2/zero)
                           :max-force 0.1
                           :max-speed 0.5}]

                ;; hippy wander
                (let [action (rand-nth [:wander :wander
                                        :pause :pause :pause
                                        :hunt
                                        :hone
                                        :attach :attach :attach
                                        ])]
                  ;(log "Action:" (str action))
                  (case action
                    :pause
                    (recur
                     (loop [n 100
                            boid boid]
                       (sprite/set-pos! sprite (:pos boid))
                       (<! (events/next-frame))

                       (if (pos? n)
                         (recur (dec n) boid)
                         boid)))

                    :attach
                    (recur
                     (let [closest (first
                                    (filter
                                     #(> (:age %) 5)
                                     (sort-by
                                      #(vec2/distance-squared
                                        (:pos %)
                                        (sprite/get-pos sprite))
                                      (:plants @game))))]
                       ;(log "closest" closest)
                       (if (not closest)
                         boid
                         (let [dest (vec2/add
                                     (:pos closest)
                                     (vec2/scale
                                      (vec2/random-unit)
                                      50))]
                           (loop [
                                  boid boid]
                             (if (pos? (vec2/get-x (:vel boid)))
                               (sprite/set-texture! sprite (:hippy-right assets))
                               (sprite/set-texture! sprite (:hippy-left assets)))
                                        ;(log n (:pos boid))
                             (sprite/set-pos! sprite (:pos boid))
                             (<! (events/next-frame))

                             (if (-> boid :pos
                                     (vec2/distance-squared dest)
                                     (> 20))
                               (recur
                                (b/arrive boid
                                          dest
                                          20))

                               (do
                                 ;; smoke mode
                                 ;; SMOKE
                                 ;(log "log shorter")

                                 ;; turn to face tree
                                 (let [dir (vec2/direction dest (:pos closest))
                                       right? (pos? (vec2/get-x dir))]
                                   (if right?
                                     (sprite/set-texture! sprite (:hippy-right-smoke assets))
                                     (sprite/set-texture! sprite (:hippy-left-smoke assets)))

                                   (m/with-sprite canvas :world
                                     [smoke (sprite/make-sprite
                                             (:smoke-1 assets)
                                             :x (.-position.x sprite)
                                             :y (- (.-position.y sprite) 0)
                                             :scale [scale scale]
                                             :xhandle 0.5
                                             :yhandle 3.0)]
                                     (loop [n 0]
                                       #_ (if (zero? (mod (int (/ n 10)) 2))
                                            (sprite/set-texture! sprite)
                                            )

                                       (sprite/set-texture! smoke
                                                            ((nth [:smoke-1 :smoke-2
                                                                   :smoke-3 :smoke-4]
                                                                  (mod (int n) 4))
                                                             assets))

                                       (<! (events/wait-time 100))


                                       (<! (events/next-frame))
                                       (if
                                           (and
                                            (pos? (mod n 50))
                                            (> (:age closest) 4)
                                            (some #(= (:id %) (:id closest))
                                                  (:plants @game))
                                            )
                                         ;; exit
                                         boid

                                         ;; negative
                                         (do

                                           ;(log "-" (count (:plants @game)) "id:" (:id closest) "closest:" closest "plants:" (str (:plants @game)))
                                           ;(log "new" (str (update closest :yield - 10)))
                                           (let [old-plant (first (filter
                                                                   #(= (:id closest) (:id %))
                                                                   (:plants @game)))

                                                 plants (disj (:plants @game) old-plant)]
                                             ;(log "--" old-plant (count plants))
                                             (when old-plant
                                               (swap! game
                                                      assoc :plants
                                                      (conj plants (update old-plant :yield
                                                                           (fn [y] (max 10 (- y 10)))))
                                                      )))
                                           ;(log "+" (count (:plants @game)))
                                           ;(log "plants:" (str (:plants @game)))
                                           (go
                                             (m/with-sprite canvas :float
                                               [minus (sprite/make-sprite
                                                       (:minus assets)
                                                       :x (.-position.x (:sprite closest))
                                                       :y (- (.-position.y (:sprite closest)) 50)
                                                       :scale scale-2
                                                       :xhandle 0.5
                                                       :yhandle 0.5)]
                                               (loop [n 100
                                                      y (- (.-position.y (:sprite closest)) 50)]
                                                 (sprite/set-pos! minus (.-position.x (:sprite closest)) y)
                                                 (sprite/set-alpha! minus (/ n 100))
                                                 (<! (events/next-frame))
                                                 (when (pos? n)
                                                   (recur (dec n) (- y 2))
                                                   )
                                                 )
                                               ))))

                                       (if
                                           (and

                                            (> (:age closest) 4)
                                            (some #(= (:id %) (:id closest))
                                                  (:plants @game))
                                            )
                                         (recur (inc n))
                                         boid))))


                                 boid)))))))

                    :hone
                    (recur
                     (let [closest (first
                                    (filter
                                     #(> (:age %) 2)
                                     (sort-by
                                      #(vec2/distance-squared
                                        (:pos %)
                                        (sprite/get-pos sprite))
                                      (:plants @game))))]
                       (if (not closest)
                         boid
                         (loop [n 400
                                boid boid]
                           (if (pos? (vec2/get-x (:vel boid)))
                             (sprite/set-texture! sprite (:hippy-right assets))
                             (sprite/set-texture! sprite (:hippy-left assets)))
                                        ;(log n (:pos boid))
                           (sprite/set-pos! sprite (:pos boid))
                           (<! (events/next-frame))

                           (if (pos? n)
                             (recur (dec n)
                                    (b/arrive boid
                                              (:pos closest)
                                              20))
                             boid)))))

                    :hunt
                    (recur
                     (loop [n 400
                            boid boid]
                       (if (pos? (vec2/get-x (:vel boid)))
                         (sprite/set-texture! sprite (:hippy-right assets))
                         (sprite/set-texture! sprite (:hippy-left assets)))
                                        ;(log n (:pos boid))
                       (sprite/set-pos! sprite (:pos boid))
                       (<! (events/next-frame))

                       (if (pos? n)
                         (recur (dec n)
                                (b/arrive boid
                                          (vec2/zero)
                                          200))
                         boid)))

                    :wander
                    (recur
                     (loop [n 300
                            boid boid]
                                        ;(log "wander" (:pos b))
                       (if (pos? (vec2/get-x (:vel boid)))
                         (sprite/set-texture! sprite (:hippy-right assets))
                         (sprite/set-texture! sprite (:hippy-left assets)))
                       (sprite/set-pos! sprite (:pos boid))
                       (<! (events/next-frame))

                       (if (pos? n)
                         (recur
                          (dec n)
                          (b/wander boid 40 10 3))
                         boid
                         ))))))))

          ;; TODO: exit this thread on death
          fly-go-thread
          (fn [{:keys [pos sprite] :as fly}]
            (go
              (sprite/set-pos! sprite pos)

              (loop []
                ;; wait for a random time before looking for target (lower cpu)
                (<! (events/wait-time (math/rand-between 1000 4000)))

                ;(log "FLY RETARGET:" (str (:plants @game)))

                (let [closest-plant
                      (first
                       (filter
                        #(> (:age %) 2)
                        (sort-by
                         #(vec2/distance-squared
                           (:pos %)
                           (sprite/get-pos sprite))
                         (:plants @game))))]
                  ;(log "close:" closest-plant)
                  (when closest-plant
                    (when (< (vec2/distance-squared
                              (sprite/get-pos sprite)
                              (:pos closest-plant))
                             (* 20 20))
                      (sound/play-sound sfx-tree-hurt 0.3 false)
                      (swap! game update :plants
                             #(-> %
                                  (disj closest-plant)
                                  (conj (update closest-plant :age
                                                (fn [x] (- x (* 500 (:growth-rate @game)))))))))

                    (loop [b {:mass 0.5
                              :pos (sprite/get-pos sprite)
                              :vel (vec2/zero)
                              :max-force 0.01
                              :max-speed 1}]
                      (sprite/set-pos! sprite (:pos b))
                      (<! (events/next-frame))
                      (when (> (vec2/distance-squared
                                (:pos b)
                                (:pos closest-plant))
                               (* 10 10))
                        (recur
                         (b/arrive b (vec2/add
                                      (vec2/vec2 0 2)
                                      (:pos closest-plant))
                                   100))))))

                (when ((:flies @game) fly)
                  ;(log "looping:" (str fly))
                  (recur))
                ))
            )

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
                                        next)))
                            )
                       plants))
            )


          ]

      ;; add the first flys
      (doall
       (for [fly (-> @game :flies)]
         (do
           (.addChild (-> canvas :layer :world) (:sprite fly))
           (fly-go-thread fly))))

      ;; add the first hippies
      (doall
       (for [hippy (-> @game :hippies)]
         (do
           ;(log "ADDING HIPPY" (:sprite hippy))
           (.addChild (-> canvas :layer :world) (:sprite hippy))
           ;(log "go thread")
           (hippy-go-thread hippy))))



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
                [caravan (sprite/make-sprite
                          (:caravan assets)
                          :scale [(* 1.5 scale) (* 1.5 scale)]
                          :x 0 :y 0
                          :xhandle 0.5 :yhandle 0.85
                          :alpha 1.0)

                 walker (sprite/make-sprite
                         (:char-1 assets)
                         :scale scale-2
                         :x 0 :y 0
                         :xhandle 0.5 :yhandle 1.0
                         :alpha 1.0
                         )]

                (let [bg-chan (chan)
                      click-chan (chan)
                      dest (atom (vec2/vec2 0.1 0.1))]

                  ;; 'thread' to handle buttons on caravan
                  (go
                    (set! (.-interactive caravan) true)
                    (set! (.-mousedown caravan)
                          ;; click on the caravan
                          (fn [ev] ;(log "caravan mousedown" ev)


                            ;; open action icons go-thread

                            (swap! game update-in [:caravan :buttons] not)


                            ))

                    ;; forever
                    (loop []
                      (<! (events/next-frame))

                      ;; buttons flips to true
                      (when (-> @game :caravan :buttons)
                        (swap! game assoc-in [:walker :buttons] false)
                        (sound/play-sound sfx-button-open 0.5 false)
                        ;; appear
                        (m/with-sprite canvas :float
                          [button (sprite/make-sprite
                                   (:button-man assets)
                                   :scale scale-2
                                   :x 0
                                   :y 0)
                           button-faster (sprite/make-sprite
                                          (:button-faster assets)
                                          :scale scale-2
                                          :x 0
                                          :y 0)
                           button-seed (sprite/make-sprite
                                        (:button-seed assets)
                                        :scale scale-2
                                        :x 0
                                        :y 0)]
                          ;; on click handlers
                          (set! (.-interactive button) true)
                          (set! (.-mousedown button)
                                ;; click on the walker
                                (fn [ev] ;(log "button mousedown" ev)
                                  (put! click-chan :man)
                                  ))

                          (set! (.-interactive button-faster) true)
                          (set! (.-mousedown button-faster)
                                ;; click on the walker
                                (fn [ev] ;(log "button mousedown" ev)
                                  (put! click-chan :faster)
                                  ))

                          (set! (.-interactive button-seed) true)
                          (set! (.-mousedown button-seed)
                                ;; click on the walker
                                (fn [ev] ;(log "button mousedown" ev)
                                  (put! click-chan :seed)
                                  ))



                          ;; TURN BUTTONS ON AND OFF
                          (let [money (:dollars @game)]
                            (sprite/set-alpha! button (if (>= money (-> @game :levels :man :cost)) 1.0 0.5))
                            (sprite/set-alpha! button-faster (if (>= money (-> @game :levels :faster :cost)) 1.0 0.5))
                            (sprite/set-alpha! button-seed (if (>= money (-> @game :levels :seed :cost)) 1.0 0.5))

                            ;; move button out and up
                            (loop [b {:mass 1 :pos
                                      (vec2/sub
                                       (sprite/get-pos caravan 20 -40)
                                       (vec2/vec2 0 50))
                                      :vel (vec2/vec2 0 -10)
                                      :max-force 1
                                      :max-speed 10}
                                   b2 {:mass 1 :pos
                                       (vec2/sub
                                        (sprite/get-pos caravan 20 -40)
                                        (vec2/vec2 0 0))
                                       :vel
                                       (vec2/rotate
                                        (vec2/vec2 0 -10)
                                        (/ Math/PI 0.5 3))
                                       :max-force 1
                                       :max-speed 10}
                                   b3 {:mass 1 :pos
                                       (sprite/get-pos caravan 20 -40)
                                       :vel (vec2/rotate
                                             (vec2/vec2 0 -10)
                                             (/ Math/PI 0.25 3))
                                       :max-force 1
                                       :max-speed 10}
                                   ]
                              (sprite/set-pos! button (:pos b))
                              (sprite/set-pos! button-faster (:pos b2))
                              (sprite/set-pos! button-seed (:pos b3))
                                        ;(<! (events/next-frame))

                              ;; keep 'arriving' while buttons is still true
                              (if (-> @game :caravan :buttons)
                                ;; buttons is still true
                                (let [[v c] (alts! #js [(events/next-frame) click-chan])]
                                  (if (= c click-chan)
                                    (case v
                                      :man
                                      ;; a button was clicked! exit and reset
                                      (if (>= money (-> @game :levels :man :cost))
                                        (do (sound/play-sound sfx-button-select 0.7 false)

                                            ;; grow and fade
                                            (loop [n 10]
                                              (when (pos? n)
                                                (effects/scale! button 1.05)
                                                (effects/scale-alpha! button 0.92)
                                                (<! (events/next-frame))
                                                (recur (dec n))))

                                            ;; user clicked MAN
                                            ;(log "man")
                                            (swap! game
                                                   #(->
                                                     %
                                                     ((-> % :levels :man :activate))
                                                     (update-in [:caravan :buttons] not)
                                                     (assoc-in [:walker :buttons] false)))
                                            ;(log "became :" (str @game))
                                            )

                                        (do
                                          (swap! game
                                                 #(-> %

                                                      (update-in [:caravan :buttons] not)
                                                      (assoc-in [:walker :buttons] false)))
                                          (sound/play-sound sfx-button-close 0.5 false)
                                          (loop [b b
                                                 b2 b2
                                                 b3 b3
                                                 n 10]
                                            (sprite/set-pos! button (:pos b))
                                            (sprite/set-pos! button-faster (:pos b2))
                                            (sprite/set-pos! button-seed (:pos b3))
                                            (<! (events/next-frame))



                                            ;; return home
                                            (when (pos? n)
                                              (recur (b/arrive b
                                                               (sprite/get-pos caravan 20 -40)
                                                               50.0)
                                                     (b/arrive b2
                                                               (sprite/get-pos caravan 20 -40)
                                                               50.0)
                                                     (b/arrive b3
                                                               (sprite/get-pos caravan 20 -40)
                                                               50.0)
                                                     (dec n)))
                                            )
                                          ))

                                      ;; select "sprayed"
                                      :faster
                                      (if (>= money (-> @game :levels :faster :cost))
                                        (do (sound/play-sound sfx-button-select 0.7 false)

                                            ;; grow and fade
                                            (loop [n 10]
                                              (when (pos? n)
                                                (effects/scale! button-faster 1.05)
                                                (effects/scale-alpha! button-faster 0.92)
                                                (<! (events/next-frame))
                                                (recur (dec n))))

                                            ;; user clicked FASTER
                                            (swap! game
                                                   #(->
                                                     %
                                                     ((-> % :levels :faster :activate))
                                                     (update-in [:caravan :buttons] not)
                                                     (assoc-in [:walker :buttons] false)
                                                     )))

                                        (do
                                          (swap! game
                                                 #(-> %
                                                      (update-in [:caravan :buttons] not)
                                                      (assoc-in [:walker :buttons] false)))

                                          (sound/play-sound sfx-button-close 0.5 false)
                                          (loop [b b
                                                 b2 b2
                                                 b3 b3
                                                 n 10]
                                            (sprite/set-pos! button (:pos b))
                                            (sprite/set-pos! button-faster (:pos b2))
                                            (sprite/set-pos! button-seed (:pos b3))
                                            (<! (events/next-frame))

                                            ;; return home
                                            (when (pos? n)
                                              (recur (b/arrive b
                                                               (sprite/get-pos caravan 20 -40)
                                                               50.0)
                                                     (b/arrive b2
                                                               (sprite/get-pos caravan 20 -40)
                                                               50.0)
                                                     (b/arrive b3
                                                               (sprite/get-pos caravan 20 -40)
                                                               50.0)
                                                     (dec n)))
                                            )
                                          ))

                                      ;; select "chopped"
                                      :seed
                                      (if (>= money (-> @game :levels :seed :cost))
                                        (do (sound/play-sound sfx-button-select 0.7 false)

                                            ;; grow and fade
                                            (loop [n 10]
                                              (when (pos? n)
                                                (effects/scale! button-seed 1.05)
                                                (effects/scale-alpha! button-seed 0.92)
                                                (<! (events/next-frame))
                                                (recur (dec n))))

                                            ;; user clicked SEED
                                            (swap! game
                                                   #(->
                                                     %
                                                     ((-> % :levels :seed :activate))
                                                     (update-in [:caravan :buttons] not)
                                                     (assoc-in [:walker :buttons] false)
                                                     )))
                                        (do
                                          (swap! game
                                                 #(-> %
                                                      (update-in [:caravan :buttons] not)
                                                      (assoc-in [:walker :buttons] false)))
                                          (sound/play-sound sfx-button-close 0.5 false)
                                          (loop [b b
                                                 b2 b2
                                                 b3 b3
                                                 n 10]
                                            (sprite/set-pos! button (:pos b))
                                            (sprite/set-pos! button-faster (:pos b2))
                                            (sprite/set-pos! button-seed (:pos b3))
                                            (<! (events/next-frame))

                                            ;; return home
                                            (when (pos? n)
                                              (recur (b/arrive b
                                                               (sprite/get-pos caravan 20 -40)
                                                               50.0)
                                                     (b/arrive b2
                                                               (sprite/get-pos caravan 20 -40)
                                                               50.0)
                                                     (b/arrive b3
                                                               (sprite/get-pos caravan 20 -40)
                                                               50.0)
                                                     (dec n)))
                                            )
                                          ))


                                      ;; unknown button clicked
                                      (do
                                        (swap! game
                                               #(-> %
                                                    (update-in [:caravan :buttons] not)
                                                    (assoc-in [:walker :buttons] false)))

                                        (sound/play-sound sfx-button-close 0.5 false)
                                        (loop [b b
                                               b2 b2
                                               b3 b3
                                               n 10]
                                          (sprite/set-pos! button (:pos b))
                                          (sprite/set-pos! button-faster (:pos b2))
                                          (sprite/set-pos! button-seed (:pos b3))
                                          (<! (events/next-frame))

                                          ;; return home
                                          (when (pos? n)
                                            (recur (b/arrive b
                                                             (sprite/get-pos caravan 20 -40)
                                                             50.0)
                                                   (b/arrive b2
                                                             (sprite/get-pos caravan 20 -40)
                                                             50.0)
                                                   (b/arrive b3
                                                             (sprite/get-pos caravan 20 -40)
                                                             50.0)
                                                   (dec n)))
                                          )
                                        ))

                                    ;; no button is clicked.
                                    (recur (b/arrive b
                                                     (vec2/sub
                                                      (sprite/get-pos caravan 20 -40)
                                                      (vec2/vec2 0 100)) 50.0)
                                           (b/arrive b2
                                                     (vec2/sub
                                                      (sprite/get-pos caravan 20 -40)
                                                      (vec2/rotate
                                                       (vec2/vec2 0 70)
                                                       (/ Math/PI 0.5 3))) 50.0)
                                           (b/arrive b3
                                                     (vec2/sub
                                                      (sprite/get-pos caravan 20 -40)
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
                                      (sprite/set-pos! button-faster (:pos b2))
                                      (sprite/set-pos! button-seed (:pos b3))
                                      (<! (events/next-frame))

                                      ;; return home
                                      (when (pos? n)
                                        (recur (b/arrive b
                                                         (sprite/get-pos caravan 20 -40)
                                                         50.0)
                                               (b/arrive b2
                                                         (sprite/get-pos caravan 20 -40)
                                                         50.0)
                                               (b/arrive b3
                                                         (sprite/get-pos caravan 20 -40)
                                                         50.0)
                                               (dec n)))
                                      ))

                                ;; buttons is no longer true
                                (do (sound/play-sound sfx-button-close 0.5 false)
                                    (loop [b b
                                           b2 b2
                                           b3 b3
                                           n 10]
                                      (sprite/set-pos! button (:pos b))
                                      (sprite/set-pos! button-faster (:pos b2))
                                      (sprite/set-pos! button-seed (:pos b3))
                                      (<! (events/next-frame))

                                      ;; return home
                                      (when (pos? n)
                                        (recur (b/arrive b
                                                         (sprite/get-pos caravan 20 -40)
                                                         50.0)
                                               (b/arrive b2
                                                         (sprite/get-pos caravan 20 -40)
                                                         50.0)
                                               (b/arrive b3
                                                         (sprite/get-pos caravan 20 -40)
                                                         50.0)
                                               (dec n)))
                                      )))
                              ))))

                      (recur)))

                  ;; 'thread' to handle buttons on walker
                  (go
                    ;; forever
                    (loop []
                      (<! (events/next-frame))

                      ;; buttons flips to true
                      (when (-> @game :walker :buttons)
                        (swap! game assoc-in [:caravan :buttons] false)
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
                                (fn [ev] ;(log "button mousedown" ev)
                                  (put! click-chan :plant)
                                  ))

                          (set! (.-interactive button-chop) true)
                          (set! (.-mousedown button-chop)
                                ;; click on the walker
                                (fn [ev] ;(log "button mousedown" ev)
                                  (put! click-chan :chop)
                                  ))

                          (set! (.-interactive button-spray) true)
                          (set! (.-mousedown button-spray)
                                ;; click on the walker
                                (fn [ev] ;(log "button mousedown" ev)
                                  (put! click-chan :spray)
                                  ))



                          (if (zero? (-> @game :seeds))
                            (sprite/set-alpha! button 0.5)
                            (sprite/set-alpha! button 1))

                          (if (< (:dollars @game) 100)
                            (sprite/set-alpha! button-spray 0.5)
                            (sprite/set-alpha! button-spray 1.0))

                          (let [closest-plant (first
                                               (filter
                                                #(> (:age %) 6)
                                                (sort-by
                                                 #(vec2/distance-squared
                                                   (:pos %)
                                                   (sprite/get-pos walker))
                                                 (:plants @game))))]
                            (if closest-plant
                              (let [distance (vec2/distance
                                              (:pos closest-plant)
                                              (sprite/get-pos walker))]

                                (if (< distance 15)
                                  (sprite/set-alpha! button-chop 1.0)
                                  (sprite/set-alpha! button-chop 0.5)))
                              (sprite/set-alpha! button-chop 0.5)
                              ))

                          ;; move button out and up
                          (loop [b {:mass 1 :pos
                                    (vec2/sub
                                     (sprite/get-pos walker)
                                     (vec2/vec2 0 50))
                                    :vel (vec2/vec2 0 -10)
                                    :max-force 1
                                    :max-speed 10}
                                 b2 {:mass 1 :pos
                                     (vec2/sub
                                      (sprite/get-pos walker)
                                      (vec2/vec2 0 0))
                                     :vel
                                     (vec2/rotate
                                      (vec2/vec2 0 -10)
                                      (/ Math/PI 0.5 3))
                                     :max-force 1
                                     :max-speed 10}
                                 b3 {:mass 1 :pos
                                     (sprite/get-pos walker)
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
                                                             (sprite/get-pos walker)
                                                             50.0)
                                                   (b/arrive b2
                                                             (sprite/get-pos walker)
                                                             50.0)
                                                   (b/arrive b3
                                                             (sprite/get-pos walker)
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

                                    ;; select "sprayed"
                                    :spray
                                    (if (< (:dollars @game) 100)
                                      ;; exit
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
                                                             (sprite/get-pos walker)
                                                             50.0)
                                                   (b/arrive b2
                                                             (sprite/get-pos walker)
                                                             50.0)
                                                   (b/arrive b3
                                                             (sprite/get-pos walker)
                                                             50.0)
                                                   (dec n)))
                                          )
                                        )

                                      ;; spray
                                      (do




                                        ;; the spray
                                        (go
                                          (sound/play-sound sfx-bug-spray 0.3 false)
                                          (m/with-sprite canvas :float
                                            [spray (sprite/make-sprite
                                                    (:spray-1 assets)
                                                    :scale scale-2
                                                    :x 0
                                                    :y 0)]

                                            (sprite/set-pos! spray
                                                             (vec2/vec2
                                                              (.-position.x walker)
                                                              (- (.-position.y walker)
                                                                 25)))

                                            ;; find all flies in collision with the spray
                                            ;(log "FLIES!!!")
                                            (let [flies (:flies @game)
                                                  touching (filter
                                                            #(< (vec2/distance-squared
                                                                 (sprite/get-pos (:sprite %))
                                                                 (sprite/get-pos spray))
                                                                ;; spray effective distance
                                                                (* 40 40))
                                                            flies)]
                                              (when (not (empty? touching))

                                                ;(log "REMOVING:" (count touching))



                                                ;; kill all the touching flies
                                                (doall (for [fly touching]
                                                         (.removeChild (-> canvas :layer :world)
                                                                       (:sprite fly))))

                                                (swap! game update :flies (fn [flies]
                                                                            (reduce disj
                                                                                    flies
                                                                                    touching)))

                                                ;; GAME LEVEL PLAY
                                                ;(log "NOW:" (count (:flies @game)))
                                                (when (zero? (count (:flies @game)))
                                                  (swap! game update :flies
                                                         (fn [flies]
                                                           (let [num-new-flies (:level @game)
                                                                 flies-to-add
                                                                 (map #(new-flies (vec2/scale (vec2/random-unit) 2500))
                                                                      (range num-new-flies))]
                                                             (doall
                                                              (for [fly flies-to-add]
                                                                (do
                                                                  (.addChild (-> canvas :layer :world)
                                                                             (:sprite fly))
                                                                  (fly-go-thread fly))))
                                                             (into flies flies-to-add)
                                                             )))
                                                  )
                                                (let [hippies-should-be (dec (:level @game))
                                                      hippies-are (count (:hippies @game))]
                                                  (when (< hippies-are
                                                           hippies-should-be)
                                                    (let [to-add (- hippies-should-be hippies-are)]
                                                      (swap! game update :hippies
                                                         (fn [hippies]
                                                           (let [
                                                                 hippies-to-add
                                                                 (map #(new-hippy (vec2/scale (vec2/random-unit) 2500))
                                                                      (range to-add))]
                                                             (doall
                                                              (for [hippy hippies-to-add]
                                                                (do
                                                                  (.addChild (-> canvas :layer :world)
                                                                             (:sprite hippy))
                                                                  (hippy-go-thread hippy))))
                                                             (into hippies hippies-to-add)
                                                             )))
                                                      )
                                                    ))

)



                                              )

                                            (let [spray-frames
                                                  [:spray-1
                                                   :spray-2
                                                   :spray-3
                                                   :spray-4
                                                   :spray-5]]
                                              (loop [f 0]
                                                (sprite/set-texture!
                                                 spray
                                                 (
                                                  (nth spray-frames f)
                                                  assets))
                                                (<! (events/wait-time 100))
                                                (when (< f (dec (count spray-frames)))
                                                  (recur (inc f)))))))

                                        ;; grow and fade
                                        (loop [n 10]
                                          (when (pos? n)
                                            (effects/scale! button-spray 1.05)
                                            (effects/scale-alpha! button-spray 0.92)
                                            (<! (events/next-frame))
                                            (recur (dec n))))

                                        (swap! game
                                               #(->
                                                 %
                                                 (update-in [:walker :buttons] not)
                                                 (assoc-in [:walker :action] :walk)
                                                 (assoc-in [:walker :chopping] nil)
                                                 (update :dollars - 100)
                                                 (assoc-in [:walker :action-count] 0)))

                                        (set-seed-text (:seeds @game) (:dollars @game))))

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

                                        (let [closest (first
                                                       (filter
                                                        (fn [pl] (> (:age pl) 6))
                                                        (sort-by
                                                         (fn [pl] (vec2/distance-squared
                                                                   (:pos pl)
                                                                   (sprite/get-pos walker)))
                                                         (:plants @game))))]

                                          ;; user clicked chop
                                          (chop-tree-go-thread closest)

                                          (swap! game
                                                 #(->
                                                   %
                                                   (update-in [:walker :buttons] not)
                                                   (assoc-in [:walker :action] :chop)
                                                   (assoc-in [:walker :chopping]
                                                             closest
                                                             )
                                                   (assoc-in [:walker :action-count] 0))))





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
                                                             (sprite/get-pos walker)
                                                             50.0)
                                                   (b/arrive b2
                                                             (sprite/get-pos walker)
                                                             50.0)
                                                   (b/arrive b3
                                                             (sprite/get-pos walker)
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
                                                           (sprite/get-pos walker)
                                                           50.0)
                                                 (b/arrive b2
                                                           (sprite/get-pos walker)
                                                           50.0)
                                                 (b/arrive b3
                                                           (sprite/get-pos walker)
                                                           50.0)
                                                 (dec n)))
                                        )
                                      ))

                                  ;; no button is clicked.
                                  (recur (b/arrive b
                                                   (vec2/sub
                                                    (sprite/get-pos walker)
                                                    (vec2/vec2 0 100)) 50.0)
                                         (b/arrive b2
                                                   (vec2/sub
                                                    (sprite/get-pos walker)
                                                    (vec2/rotate
                                                     (vec2/vec2 0 70)
                                                     (/ Math/PI 0.5 3))) 50.0)
                                         (b/arrive b3
                                                   (vec2/sub
                                                    (sprite/get-pos walker)
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
                                                       (sprite/get-pos walker)
                                                       50.0)
                                             (b/arrive b2
                                                       (sprite/get-pos walker)
                                                       50.0)
                                             (b/arrive b3
                                                       (sprite/get-pos walker)
                                                       50.0)
                                             (dec n)))
                                    )))
                            )))

                      (recur)))

                  (set! (.-interactive walker) true)
                  (set! (.-mousedown walker)
                        ;; click on the walker
                        (fn [ev] ;(log "mousedown" ev)


                          ;; open action icons go-thread

                          (swap! game update-in [:walker :buttons] not)


                          ))
                  (set! (.-interactive (-> canvas :stage)) true)
                  (set! (.-mousedown (-> canvas :stage)) (fn [ev] ;(log "MD" ev)
                                                           ))

                  (set! (.-interactive (-> canvas :layer :bg)) true)
                  (set! (.-mousedown (-> canvas :layer :bg))
                        (fn [ev] ;(log "BG" ev)
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



                          (swap! game #(-> %
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
                    (swap! game #(-> %
                                     (update-in [:walker :action-count] inc)
                                     (update :frame inc)
                                     (update :plants update-plants)
                                     (update :flies update-flies)))

                    (<! (events/next-frame))

                    ;; planted
                    (let [{{:keys [action action-count]} :walker} @game]
                      (when (and (= :plant action)
                                 (= (:plant-length @game) action-count))
                        ;(log "PLANT")

                        ;;
                        ;; plant a plant?
                        ;;
                        (let [seeds (:seeds @game)]
                          ;(log "seeds:" seeds)
                          (if (zero? seeds)
                            ;; deny
                            (do (sound/play-sound sfx-tree-not-planted 0.5 false)
                                (swap! game
                                       #(-> %
                                            (assoc-in [:walker :action] :walk)
                                            (assoc-in [:walker :action-count] 0))))

                            (let [newplant (assoc (new-plant (:pos b))
                                                  :growth-rate (:growth-rate @game))]
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

                                        ;(update-tree-chop)

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
