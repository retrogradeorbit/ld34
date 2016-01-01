(ns ld34.caravan
  (:require [infinitelives.pixi.sprite :as sprite]
            [infinitelives.utils.vec2 :as vec2]
            [infinitelives.utils.events :as events]
            [infinitelives.utils.sound :as sound])
  )

(def alpha-off 0.5)
(def alpha-on 1.0)

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

(defn button-enable-disable [game money]
  (sprite/set-alpha!
   button
   (if (>= money (-> @game :levels :man :cost))
     alpha-on
     alpha-off))
  (sprite/set-alpha!
   button-faster
   (if (>= money (-> @game :levels :faster :cost))
     alpha-on
     alpha-off))
  (sprite/set-alpha!
   button-seed
   (if (>= money (-> @game :levels :seed :cost))
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

(defn close-buttons [game but1 but2 but3]
  (go
    (sound/play-sound :button-close 0.5 false)
    (swap! game
           #(-> %
                (update-in [:caravan :buttons] not)
                (assoc-in [:walker :buttons] false)))

    (loop [b1 b1
           b2 b2
           b3 b3
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

(defn buttons-open [but1 but2 but3]
  (loop [b1 (button-boid caravan 50 0)
         b2 (button-boid caravan 0 (/ Math/PI 0.5 3))
         b3 (button-boid caravan 0 (/ Math/PI 0.25 3))]
    (sprite/set-pos! but1 (:pos b1))
    (sprite/set-pos! but2 (:pos b2))
    (sprite/set-pos! but3 (:pos b3))

    ;; keep 'arriving' while buttons is still true
    (if (-> @game :caravan :buttons)
      ;; buttons is still true
      (let [[v c] (alts! #js [(events/next-frame) click-chan])]
        (if (= c click-chan)
          (case v
            :man
            ;; the man button was clicked! exit and reset
            (if (>= money (-> @game :levels :man :cost))
              (do
                ;; man button is active with available money
                (sound/play-sound :button-select 0.7 false)

                ;; grow and fade
                (<! (grow-and-fade but1))

                ;; user clicked MAN
                (swap! game
                       #(->
                         %
                         ((-> % :levels :man :activate))
                         (update-in [:caravan :buttons] not)
                         (assoc-in [:walker :buttons] false))))

              ;; man button inactive and clicked
              (<! (close-buttons)))

            ;; select "sprayed"
            :faster
            (if (>= money (-> @game :levels :faster :cost))
              (do (sound/play-sound :button-select 0.7 false)

                  ;; grow and fade
                  (<! (grow-and-fade button-faster))

                  ;; user clicked FASTER
                  (swap! game
                         #(->
                           %
                           ((-> % :levels :faster :activate))
                           (update-in [:caravan :buttons] not)
                           (assoc-in [:walker :buttons] false)
                           )))

              ;; no money for faster
              (<! (close-buttons)))

            ;; select "chopped"
            :seed
            (if (>= money (-> @game :levels :seed :cost))
              (do (sound/play-sound :button-select 0.7 false)

                  ;; grow and fade
                  (<! (grow-and-fade button-seed))

                  ;; user clicked SEED
                  (swap! game
                         #(->
                           %
                           ((-> % :levels :seed :activate))
                           (update-in [:caravan :buttons] not)
                           (assoc-in [:walker :buttons] false)
                           )))

              ;; not enough money
              (<! (close-buttons)))

            ;; unknown button clicked
            (<! (close-buttons)))

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
      (do
        (sound/play-sound :button-close 0.5 false)
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
                   (dec n))))))))


(defn appear []
  (m/with-sprite canvas :float
    [button (sprite/make-sprite
             (:button-man assets)
             :scale scale-2
             :interactive true
             :mousedown #(put! click-chan :man))
     button-faster (sprite/make-sprite
                    (:button-faster assets)
                    :scale scale-2
                    :interactive true
                    :mousedown #(put! click-chan :faster))
     button-seed (sprite/make-sprite
                  (:button-seed assets)
                  :scale scale-2
                  :interactive true
                  :mousedown #(put! click-chan :seed))]

    ;; turn buttons on and off depending on cost
    (let [money (:dollars @game)]
      (button-enable-disable game money)

      ;; move button out and up
      (<! (buttons-open button button-faster button-seed))


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
        )))
  )

;; 'thread' to handle buttons on caravan
(defn button-thread [game caravan]
  (go
    (set! (.-interactive caravan) true)
    (set! (.-mousedown caravan)
          ;; click on the caravan
          (fn [ev]
            ;; open action icons go-thread
            (swap! game update-in [:caravan :buttons] not)))

    ;; forever
    (loop []
      (<! (events/next-frame))

      ;; buttons flips to true
      (when (-> @game :caravan :buttons)
        (swap! game assoc-in [:walker :buttons] false)
        (sound/play-sound :button-open 0.5 false)

        ;; appear
        (appear))

      (recur))))
