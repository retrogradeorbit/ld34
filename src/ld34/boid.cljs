(ns ld34.boid
  (:require [infinitelives.utils.vec2 :as vec2])
)

;; functions for boid like behavoir

(def example
  {:mass 10.0
   :pos (vec2/vec2 0 0)
   :vel (vec2/vec2 0 0)
   :max-force 1.0
   :max-speed 4.0})


(defn seek [{:keys [mass pos vel max-force max-speed] :as boid} target]
  (let [desired-vel
        (-> pos
            (vec2/direction target)
            (vec2/scale max-speed))
        steering (-> desired-vel
                     (vec2/sub vel)
                     (vec2/truncate max-force))
        accel (vec2/scale steering (/ 1 mass))
        new-vel (-> accel
                    (vec2/add vel)
                    (vec2/truncate max-speed))

        new-pos (vec2/add pos new-vel)]
    (assoc boid
           :pos new-pos
           :vel new-vel)))

(defn arrive [{:keys [mass pos vel max-force max-speed] :as boid} target slowing-distance]
  (let [target-offset (vec2/sub target pos)
        distance (vec2/magnitude target-offset)
        ramped-speed (* max-speed (/ distance slowing-distance))
        clipped-speed (min ramped-speed max-speed)

        desired-vel (vec2/scale target-offset (/ clipped-speed distance))

        steering (-> desired-vel
                     (vec2/sub vel)
                     (vec2/truncate max-force))
        accel (vec2/scale steering (/ 1 mass))
        new-vel (-> accel
                    (vec2/add vel)
                    (vec2/truncate max-speed))

        new-pos (vec2/add pos new-vel)]
    (assoc boid
           :pos new-pos
           :vel new-vel)))

(defn wander [:keys ])
