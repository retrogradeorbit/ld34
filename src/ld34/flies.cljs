(ns ld34.flies
  (:require [infinitelives.pixi.sprite :as sprite]
            [infinitelives.utils.vec2 :as vec2])
  )

(defn- texture-cycle [texture-list frame frame-length]
  (let [total-frames (count texture-list)]
    (texture-list
     (mod
      (int (/ frame frame-length))
      total-frames))))

(defn make [assets pos]
  {:pos pos
   :sprite (sprite/make-sprite (:flies-1 assets)
                               :x (vec2/get-x pos)
                               :y (vec2/get-y pos)
                               :scale 6
                               :xhandle 0.5
                               :yhandle 1.0)})
(defn update [game assets flies]
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
