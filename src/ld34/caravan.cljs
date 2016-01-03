(ns ld34.caravan
  (:require [infinitelives.pixi.sprite :as sprite]
            [infinitelives.utils.vec2 :as vec2]
            [infinitelives.utils.events :as events]
            [infinitelives.utils.sound :as sound]
            [ld34.boid :as b]
            [ld34.game :as g]
            [ld34.effects :as effects]
            [cljs.core.async :refer [<! chan put!]])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [infinitelives.pixi.macros :as m]))

(def alpha-off 0.5)
(def alpha-on 1.0)

(def scale-2 6)

(def base-button
  {:mass 1
   :max-force 1
   :max-speed 10})

(defn button-boid [caravan yoff angle]
  (assoc base-button
         :pos (vec2/sub
             (sprite/get-pos caravan 20 -40)
             (vec2/vec2 0 yoff))
         :vel (vec2/rotate
              (vec2/vec2 0 -10)
              angle)))

(defn button-enable-disable [money button button-faster button-seed]
  (sprite/set-alpha!
   button
   (if (>= money (-> @g/game :levels :man :cost))
     alpha-on
     alpha-off))
  (sprite/set-alpha!
   button-faster
   (if (>= money (-> @g/game :levels :faster :cost))
     alpha-on
     alpha-off))
  (sprite/set-alpha!
   button-seed
   (if (>= money (-> @g/game :levels :seed :cost))
     alpha-on
     alpha-off)))

(defn grow-and-fade [button]
  (go
    (loop [n 10]
      (when (pos? n)
        (effects/scale! button 1.05)
        (effects/scale-alpha! button 0.92)
        (<! (events/next-frame))
        (recur (dec n))))))

(defn close-buttons [caravan but1 but2 but3 boid1 boid2 boid3]
  (go
    (sound/play-sound :button-close 0.5 false)
    (swap! g/game
           #(-> %
                (assoc-in [:caravan :buttons] false)
                (assoc-in [:walker :buttons] false)))

    (loop [b1 boid1
           b2 boid2
           b3 boid3
           n 10]
      (sprite/set-pos! but1 (:pos b1))
      (sprite/set-pos! but2 (:pos b2))
      (sprite/set-pos! but3 (:pos b3))
      (<! (events/next-frame))

      ;; return home
      (when (pos? n)
        (recur
         (b/arrive
          b1
          (sprite/get-pos caravan 20 -40)
          50.0)
         (b/arrive
          b2
          (sprite/get-pos caravan 20 -40)
          50.0)
         (b/arrive
          b3
          (sprite/get-pos caravan 20 -40)
          50.0)
         (dec n))))))

(defn activate! [type]
  (swap! g/game
         #(->
           %
           ((-> % :levels type :activate))
           (assoc-in [:caravan :buttons] true)
           (assoc-in [:walker :buttons] false))))

(defn buttons-open [money click-chan caravan button-man button-faster button-seed]
  (go
    (loop [boid-man (button-boid caravan 50 0)
           boid-faster (button-boid caravan 0 (/ Math/PI 0.5 3))
           boid-seed (button-boid caravan 0 (/ Math/PI 0.25 3))]
      (sprite/set-pos! button-man (:pos boid-man))
      (sprite/set-pos! button-faster (:pos boid-faster))
      (sprite/set-pos! button-seed (:pos boid-seed))

      ;; keep 'arriving' while buttons is still true
      (if (-> @g/game :caravan :buttons)
        ;; buttons is still true
        (let [[v c] (alts! #js [(events/next-frame) click-chan])]
          (if (= c click-chan)
            (case v
              :man
              (if (>= money (-> @g/game :levels :man :cost))
                (do
                  (sound/play-sound :button-select 0.7 false)
                  (<! (grow-and-fade button-man))
                  (activate! :man))

                ;; man button inactive and clicked
                (<! (close-buttons caravan button-man button-faster button-seed boid-man boid-faster boid-seed)))

              :faster
              (if (>= money (-> @g/game :levels :faster :cost))
                (do
                  (sound/play-sound :button-select 0.7 false)
                  (<! (grow-and-fade button-faster))
                  (activate! :faster))

                ;; no money for faster
                (<! (close-buttons caravan button-man button-faster button-seed boid-man boid-faster boid-seed)))

              :seed
              (if (>= money (-> @g/game :levels :seed :cost))
                (do
                  (sound/play-sound :button-select 0.7 false)
                  (<! (grow-and-fade button-seed))
                  (activate! :seed))

                ;; not enough money
                (<! (close-buttons caravan button-man button-faster button-seed boid-man boid-faster boid-seed)))

              ;; unknown button clicked
              (<! (close-buttons caravan button-man button-faster button-seed boid-man boid-faster boid-seed)))

            ;; no button is clicked.
            (recur (b/arrive boid-man
                             (vec2/sub
                              (sprite/get-pos caravan 20 -40)
                              (vec2/vec2 0 100)) 50.0)
                   (b/arrive boid-faster
                             (vec2/sub
                              (sprite/get-pos caravan 20 -40)
                              (vec2/rotate
                               (vec2/vec2 0 70)
                               (/ Math/PI 0.5 3))) 50.0)
                   (b/arrive boid-seed
                             (vec2/sub
                              (sprite/get-pos caravan 20 -40)
                              (vec2/rotate
                               (vec2/vec2 0 70)
                               (/ Math/PI 0.25 3))) 50.0))))

        ;; buttons has turned false
        (<! (close-buttons caravan button-man button-faster button-seed boid-man boid-faster boid-seed))))))


(defn appear [click-chan caravan]
  (go
    (m/with-layered-sprite
      [button :float (sprite/make-sprite
                      :button-man
                      :scale scale-2
                      :interactive true
                      :mousedown #(put! click-chan :man))
       button-faster :float (sprite/make-sprite
                             :button-faster
                             :scale scale-2
                             :interactive true
                             :mousedown #(put! click-chan :faster))
       button-seed :float (sprite/make-sprite
                           :button-seed
                           :scale scale-2
                           :interactive true
                           :mousedown #(put! click-chan :seed))]

      ;; turn buttons on and off depending on cost
      (let [money (:dollars @g/game)]
        (button-enable-disable money button button-faster button-seed)

        ;; move button out and up
        (<! (buttons-open money click-chan caravan button button-faster button-seed))))))

;; 'thread' to handle buttons on caravan
(defn button-thread [caravan click-chan]
  (go
    (set! (.-interactive caravan) true)
    (set! (.-mousedown caravan)
          ;; click on the caravan
          (fn [ev]
            ;; open action icons go-thread
            (swap! g/game update-in [:caravan :buttons] not)))

    ;; forever
    (loop []
      (<! (events/next-frame))

      ;; buttons flips to true
      (when (-> @g/game :caravan :buttons)
        (swap! g/game assoc-in [:walker :buttons] false)
        (sound/play-sound :button-open 0.5 false)

        ;; appear
        (<! (appear click-chan caravan)))

      (recur))))
