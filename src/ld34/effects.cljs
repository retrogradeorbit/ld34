(ns ld34.effects
  (:require [infinitelives.pixi.sprite :as sprite]))

(defn scale! [spr f]
  (let [scale (.-scale spr)
        scale-x (.-x scale)
        scale-y (.-y scale)]
    (sprite/set-scale! spr (* f scale-x) (* f scale-y))))

(defn scale-alpha! [spr f]
  (let [alpha (.-alpha spr)]
    (sprite/set-alpha! spr (* f alpha))))

(defn shake [amplitude freq dampening frame]
  (*
   amplitude
   (Math/sin (* frame freq))
   (Math/exp (- (* dampening frame)))))
