(ns ld34.flies)

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
