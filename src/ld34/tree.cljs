(ns ld34.tree)

(defn update-tree-chop [game]
  (when (-> @game :walker :chopping)
    (let [frame (-> @game :walker :action-count)
          plant (-> @game :walker :chopping)
          [x y] (sprite/get-xy (:sprite plant))
          left-right (= 0 (mod (int (/ frame 5)) 2))
          intensity (mod (/ frame 30) 4)]
      (if left-right
        (sprite/set-pos! (:sprite plant) (+ x intensity) y)
        (sprite/set-pos! (:sprite plant) (- x intensity) y)))))

(defn chop-tree-go-thread [game {:keys [pos sprite id yield] :as tree}]
  (go
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
                  (sprite/set-pos! sprite (+
                                           (vec2/get-x pos)
                                           (effects/shake 10 2 0.07 f))
                                   (vec2/get-y pos))
                  (<! (events/next-frame))
                  (recur (inc f))))
              (recur (inc chop-num)))))))))
