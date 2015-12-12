(ns ^:figwheel-always ld34.core
    (:require [infinitelives.pixi.canvas :as canv]
              [infinitelives.pixi.resources :as resources]
              [infinitelives.pixi.sprite :as sprite]
              [infinitelives.utils.events :as events]
              [infinitelives.utils.console :refer [log]]
              [infinitelives.pixi.texture :as texture]
              [infinitelives.pixi.font :as font]
              [infinitelives.utils.math :as math]
              [infinitelives.utils.sound :as sound]
              [infinitelives.utils.dom :as dom]
              [ld34.assets :as a]
              [cljs.core.async :refer [<!]]
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
                                    :fill "#ffffff"
                                    ;:stroke "#505050"
                                    ;:strokeThickness 1
                                    ))

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
                                        ;"sfx/splash-screen.ogg"
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
          ]

      (sprite/set-alpha! test-text 0.0)
      (sprite/set-scale! test-text 5)
                                        ;(set! (.-filters test-text) #js [(shaders/make-colour-bars)])
      (.addChild (-> canvas :layer :ui) test-text)
      (resources/fadein test-text :duration 5)
      (go (loop [n 0]
            (let [h (.-innerHeight js/window)
                  hh (/ h 2)
                  qh (/ h 4)]
              (sprite/set-pos! test-text 0 ;-200
                               (+ 0 (- (* 0.1 qh (Math/sin (* 0.1 n))) qh))
                               ))
            (<! (events/next-frame))
            (recur (inc n))))



      (m/with-sprite-set canvas :bg
        [tests (for [n (range 150)]
                 (sprite/make-sprite
                  ((rand-nth [:ground-1 :ground-2]) assets)
                  :scale [(* 2 scale) (* 2 scale)]
                  :x (math/rand-between -1000 1000)
                  :y (math/rand-between -1000 1000)
                  :xhandle 0.5 :yhandle 0.5
                  :alpha 1.0))]

        (m/with-sprite-set canvas :bg
          [grass (for [n (range 300)]
                   (sprite/make-sprite
                    ((rand-nth [:grass-1 :grass-2 :grass-3]) assets)
                    :scale [(* 2 scale) (* 2 scale)]
                    :x (math/rand-between -1000 1000)
                    :y (math/rand-between -1000 1000)
                    :xhandle 0.5 :yhandle 0.5
                    :alpha 1.0)
                   )]

          (m/with-sprite-set canvas :world
            [trees (for [n (range 500)]
                     (sprite/make-sprite
                      ((rand-nth [:tree-1 :tree-2 :tree-3 :tree-4 :tree-5 :tree-6]) assets)
                      :scale [(* 2 scale) (* 2 scale)]
                      :x (math/rand-between -1000 1000)
                      :y (math/rand-between -1000 1000)
                      :xhandle 0.5 :yhandle 1.0
                      :alpha 1.0
                      ))]

            (.sort (.-children (-> canvas :layer :world)) depth-compare )

            (println tests)
            (.log js/console tests)

            (<! (events/wait-time 100000))))

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
