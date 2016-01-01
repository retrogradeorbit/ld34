(ns ld34.tree
  (:require [infinitelives.pixi.sprite :as sprite]
            [infinitelives.utils.sound :as sound]
            [infinitelives.utils.events :as events]
            [infinitelives.utils.vec2 :as vec2]
            [cljs.core.async :refer [<! chan put!]]
            [ld34.effects :as effects])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn update-chop [game]
  (when (-> @game :walker :chopping)
    (let [frame (-> @game :walker :action-count)
          plant (-> @game :walker :chopping)
          [x y] (sprite/get-xy (:sprite plant))
          left-right (= 0 (mod (int (/ frame 5)) 2))
          intensity (mod (/ frame 30) 4)]
      (if left-right
        (sprite/set-pos! (:sprite plant) (+ x intensity) y)
        (sprite/set-pos! (:sprite plant) (- x intensity) y)))))

(defn- tree-chopped-down [game canvas {:keys [yield sprite]} set-seed-text]
  (sound/play-sound :tree-harvest 0.5 false)
  (.removeChild (-> canvas :layer :world) sprite)

  (swap! game
         #(-> %
              (assoc-in [:walker :action] :walk)
              (update :plants disj (first (filter
                                           (fn [pl]
                                             (= (:sprite pl) sprite))
                                           (:plants %))))
              (update :dollars + (* yield (:plant-multiplier %)))
              (update :seeds + (:max-seeds %))))
  (set-seed-text (:seeds @game) (:dollars @game)))

(defn- tree-chopping [game {:keys [sprite pos]}]
  (go
    ;(sprite/set-pos! sprite pos)
    (<! (events/wait-time 30))

    (sound/play-sound :tree-chop 0.5 false)
    (loop [f 0]
      (when (< f (:chop-length @game))
        (sprite/set-pos! sprite (+
                                 (vec2/get-x pos)
                                 (effects/shake 10 2 0.07 f))
                         (vec2/get-y pos))
        (<! (events/next-frame))
        (recur (inc f))))))

(defn chop-tree-go-thread [game canvas {:keys [pos sprite id yield] :as tree} set-seed-text]
  (go
    ;; tree chopping
    (loop [chop-num 0]
      (when-not (= :walk (-> @game :walker :action))
        (<! (tree-chopping game tree))
        (when (< chop-num (:chop-num @game))
          (recur (inc chop-num)))))

    ;; finished chopping down
    (when-not (= :walk (-> @game :walker :action))
      (tree-chopped-down game canvas tree set-seed-text))))
