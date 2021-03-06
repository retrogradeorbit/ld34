(ns ld34.flies
  (:require [infinitelives.pixi.sprite :as sprite]
            [infinitelives.utils.vec2 :as vec2]
            [infinitelives.utils.math :as math]
            [infinitelives.utils.events :as events]
            [infinitelives.utils.sound :as sound]
            [ld34.boid :as b]
            [ld34.game :as g]
            )
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [infinitelives.pixi.macros :as m]
                   )
  )

(defn- texture-cycle [texture-list frame frame-length]
  (let [total-frames (count texture-list)]
    (texture-list
     (mod
      (int (/ frame frame-length))
      total-frames))))

(defn make [pos]
  {:pos pos
   :sprite (sprite/make-sprite :flies-1
                               :x (vec2/get-x pos)
                               :y (vec2/get-y pos)
                               :scale 6
                               :xhandle 0.5
                               :yhandle 1.0)})
(defn update-fly [flies]
  (doall (for [fly flies]
           (sprite/set-texture!
            (:sprite fly)
            (texture-cycle [:flies-1
                            :flies-2
                            :flies-3
                            :flies-4 ]
                           (:frame @g/game)
                           5))))
  (into #{} flies))

(defn go-thread
  [{:keys [pos sprite] :as fly}]
  (go
    (sprite/set-pos! sprite pos)

    (loop []
      ;; wait for a random time before looking for target (lower cpu)
      (<! (events/wait-time (math/rand-between 1000 4000)))

      (let [closest-plant
            (first
             (filter
              #(> (:age %) 2)
              (sort-by
               #(vec2/distance-squared
                 (:pos %)
                 (sprite/get-pos sprite))
               (:plants @g/game))))]
                                        ;(log "close:" closest-plant)
        (when closest-plant
          (when (< (vec2/distance-squared
                    (sprite/get-pos sprite)
                    (:pos closest-plant))
                   (* 20 20))
            (sound/play-sound :tree-hurt 0.3 false)
            (swap! g/game update :plants
                   #(-> %
                        (disj closest-plant)
                        (conj (update closest-plant :age
                                      (fn [x] (- x (* 500 (:growth-rate @g/game)))))))))

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

      (when ((:flies @g/game) fly)
        (recur)))))
