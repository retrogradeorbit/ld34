(ns ld34.walker
  (:require [infinitelives.pixi.sprite :as sprite]
            [infinitelives.utils.vec2 :as vec2]
            [infinitelives.utils.console :refer [log]]
            [infinitelives.utils.events :as events]
            [infinitelives.utils.sound :as sound]
            [ld34.boid :as b]
            [ld34.effects :as effects]
            [cljs.core.async :refer [<! chan put! alts!]])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [infinitelives.pixi.macros :as m]
                   )
  )

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
             (sprite/get-pos caravan)
             (vec2/vec2 0 yoff))
         :vel (vec2/rotate
              (vec2/vec2 0 -10)
              angle)))

(defn button-enable-disable [game walker button button-chop button-spray]
  (sprite/set-alpha! button
                     (if (zero? (-> @game :seeds))
                       alpha-off alpha-on))

  (sprite/set-alpha! button-spray
                     (if (< (-> @game :dollars) 100)
                       alpha-off alpha-on))

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
          (sprite/set-alpha! button-chop alpha-on)
          (sprite/set-alpha! button-chop alpha-off)))
      (sprite/set-alpha! button-chop alpha-off))))

(defn grow-and-fade [button]
  (go
    (loop [n 10]
      (when (pos? n)
        (effects/scale! button 1.05)
        (effects/scale-alpha! button 0.92)
        (<! (events/next-frame))
        (recur (dec n))))))

(defn close-buttons [game caravan but1 but2 but3 boid1 boid2 boid3]
  (go
    (sound/play-sound :button-close 0.5 false)
    (swap! game
           #(-> %
                (assoc-in [:walker :buttons] false)
                (assoc-in [:caravan :buttons] false)))

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
          (sprite/get-pos caravan)
          50.0)
         (b/arrive
          b2
          (sprite/get-pos caravan)
          50.0)
         (b/arrive
          b3
          (sprite/get-pos caravan)
          50.0)
         (dec n))))))

(defn activate! [game type]
  (swap! game
         #(->
           %
           ((-> % :levels type :activate))
           (assoc-in [:walker :buttons] true)
           (assoc-in [:caravan :buttons] false))))

(defn buttons-open [game money click-chan caravan button-plant button-chop button-spray]
  (go
    (loop [boid-plant (button-boid caravan 50 0)
           boid-chop (button-boid caravan 0 (/ Math/PI 0.5 3))
           boid-spray (button-boid caravan 0 (/ Math/PI 0.25 3))]
      (sprite/set-pos! button-plant (:pos boid-plant))
      (sprite/set-pos! button-chop (:pos boid-chop))
      (sprite/set-pos! button-spray (:pos boid-spray))

      ;; keep 'arriving' while buttons is still true
      (if (-> @game :walker :buttons)
        ;; buttons is still true
        (let [[v c] (alts! #js [(events/next-frame) click-chan])]
          (if (= c click-chan)
            (case v
              :plant
              (if (zero? (-> @game :seeds))
                ;; man button inactive and clicked
                (<! (close-buttons game caravan button-plant button-chop button-spray boid-plant boid-chop boid-spray))

                ;; plant
                (do
                  (sound/play-sound :button-select 0.7 false)
                  (<! (grow-and-fade button-plant))
                  (swap! game
                         #(->
                           %
                           (update-in [:walker :buttons] not)
                           (assoc-in [:walker :action] :plant)
                           (assoc-in [:walker :action-count] 0)))))

              :spray
              (if (>= money (-> @game :levels :faster :cost))
                (do
                  (sound/play-sound :button-select 0.7 false)
                  (<! (grow-and-fade button-chop))
                  (activate! game :faster))

                ;; no money for faster
                (<! (close-buttons game caravan button-plant button-chop button-spray boid-plant boid-chop boid-spray)))

              :chop
              (if (>= money (-> @game :levels :seed :cost))
                (do
                  (sound/play-sound :button-select 0.7 false)
                  (<! (grow-and-fade button-spray))
                  (activate! game :seed))

                ;; not enough money
                (<! (close-buttons game caravan button-plant button-chop button-spray boid-plant boid-chop boid-spray)))

              ;; unknown button clicked
              (<! (close-buttons game caravan button-plant button-chop button-spray boid-plant boid-chop boid-spray)))

            ;; no button is clicked.
            (recur (b/arrive boid-plant
                             (vec2/sub
                              (sprite/get-pos caravan)
                              (vec2/vec2 0 100)) 50.0)
                   (b/arrive boid-chop
                             (vec2/sub
                              (sprite/get-pos caravan)
                              (vec2/rotate
                               (vec2/vec2 0 70)
                               (/ Math/PI 0.5 3))) 50.0)
                   (b/arrive boid-spray
                             (vec2/sub
                              (sprite/get-pos caravan)
                              (vec2/rotate
                               (vec2/vec2 0 70)
                               (/ Math/PI 0.25 3))) 50.0))))

        ;; buttons has turned false
        (<! (close-buttons game caravan button-plant button-chop button-spray boid-plant boid-chop boid-spray))))))


(defn appear [assets click-chan canvas game walker]
  (go
    (m/with-sprite canvas :float
      [button-plant (sprite/make-sprite
               (:button-grow assets)
               :scale scale-2
               :interactive true
               :mousedown #(put! click-chan :plant))
       button-chop (sprite/make-sprite
                      (:button-chop assets)
                      :scale scale-2
                      :interactive true
                      :mousedown #(put! click-chan :chop))
       button-spray (sprite/make-sprite
                    (:button-spray assets)
                    :scale scale-2
                    :interactive true
                    :mousedown #(put! click-chan :spray))]

      ;; turn buttons on and off depending on cost
      (let [money (:dollars @game)]
        (button-enable-disable game walker button-plant button-chop button-spray)

        ;; move button out and up
        (<! (buttons-open game money click-chan walker button-plant button-chop button-spray))))))

;; 'thread' to handle buttons on caravan
(defn button-thread [game canvas walker assets click-chan]
  (go
    (set! (.-interactive walker) true)
    (set! (.-mousedown walker)
          ;; click on the walker
          (fn [ev]
            ;; open action icons go-thread
            (swap! game update-in [:walker :buttons] not)))

    ;; forever
    (loop []
      (<! (events/next-frame))

      ;; buttons flips to true
      (when (-> @game :walker :buttons)
        (swap! game assoc-in [:caravan :buttons] false)
        (sound/play-sound :button-open 0.5 false)

        ;; appear
        (<! (appear assets click-chan canvas game walker)))

      (recur))))
