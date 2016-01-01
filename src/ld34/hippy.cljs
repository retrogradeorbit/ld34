(ns ld34.hippy
  (:require [infinitelives.utils.events :as events]
            [infinitelives.utils.vec2 :as vec2]
            [infinitelives.pixi.sprite :as sprite]
            [ld34.boid :as b])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [infinitelives.pixi.macros :as m])
)

(defn hippy-go-thread [game canvas assets {:keys [pos sprite] :as hippy}]
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
                   (sprite/set-pos! sprite (:pos boid))
                   (<! (events/next-frame))

                   (if (-> boid :pos
                           (vec2/distance-squared dest)
                           (> 20))
                     (recur (b/arrive boid dest 20))

                     (do
                       ;; smoke mode

                       ;; turn to face tree
                       (let [dir (vec2/direction dest (:pos closest))
                             right? (pos? (vec2/get-x dir))]
                         (if right?
                           (sprite/set-texture! sprite (:hippy-right-smoke assets))
                           (sprite/set-texture! sprite (:hippy-left-smoke assets)))

                         (m/with-sprite canvas :world
                           [smoke (sprite/make-sprite
                                   (:smoke-1 assets)
                                   :x (sprite/get-x sprite)
                                   :y (- (sprite/get-y sprite) 0)
                                   :scale 3
                                   :xhandle 0.5
                                   :yhandle 3.0)]
                           (loop [n 0]
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
                                        (:plants @game)))
                               ;; exit
                               boid

                               ;; negative
                               (do
                                 (let [old-plant (first (filter
                                                         #(= (:id closest) (:id %))
                                                         (:plants @game)))

                                       plants (disj (:plants @game) old-plant)]

                                   (when old-plant
                                     (swap! game
                                            assoc :plants
                                            (conj plants (update old-plant :yield
                                                                 (fn [y] (max 10 (- y 10))))))))

                                 (go
                                   (m/with-sprite canvas :float
                                     [minus (sprite/make-sprite
                                             (:minus assets)
                                             :x (sprite/get-x (:sprite closest))
                                             :y (- (sprite/get-y (:sprite closest)) 50)
                                             :scale 6
                                             :xhandle 0.5
                                             :yhandle 0.5)]
                                     (loop [n 100
                                            y (- (sprite/get-y (:sprite closest)) 50)]
                                       (sprite/set-pos! minus (sprite/get-x (:sprite closest)) y)
                                       (sprite/set-alpha! minus (/ n 100))
                                       (<! (events/next-frame))
                                       (when (pos? n)
                                         (recur (dec n) (- y 2))))))))

                             (if
                                 (and

                                  (> (:age closest) 4)
                                  (some #(= (:id %) (:id closest))
                                        (:plants @game)))
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
             (if (pos? (vec2/get-x (:vel boid)))
               (sprite/set-texture! sprite (:hippy-right assets))
               (sprite/set-texture! sprite (:hippy-left assets)))
             (sprite/set-pos! sprite (:pos boid))
             (<! (events/next-frame))

             (if (pos? n)
               (recur
                (dec n)
                (b/wander boid 40 10 3))
               boid))))))))
