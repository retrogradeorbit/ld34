(ns ^:figwheel-always ld34.shaders
    (:require [infinitelives.pixi.canvas :as canv]
              [infinitelives.pixi.resources :as resources]
              [infinitelives.pixi.sprite :as sprite]
              [infinitelives.utils.events :as events]
              [infinitelives.utils.console :refer [log]]
              [infinitelives.pixi.texture :as texture]
              [infinitelives.pixi.font :as font]
              [infinitelives.utils.math :as math]
              [infinitelives.utils.sound :as sound]
              ;[splash.macros :refer-macros [ignore]]
              [cljs.core.async :refer [<!]]
              [PIXI])
    (:require-macros [cljs.core.async.macros :refer [go]]
                     [ld34.macros :as macros]
                     ))

;;
;; Horizontal Hue colour bars moving vertically
;;
(defn make-colour-bars []
  (let [f
        (PIXI/AbstractFilter.
         #js [
              "
precision mediump float;
varying vec2 vTextureCoord;
varying vec4 vColor;
uniform sampler2D uSampler;
uniform float time;
// hue,saturation,value -> red,green,blue
vec3 hsv2rgb(vec3 c)
{
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}
// shader mainline
void main( void ) {
    vec4 col = texture2D(uSampler, vTextureCoord);
    float r=col.r;
    float g=col.g;
    float b=col.b;
    // is our hue magenta?
    bool mag = ((r-b)<0.1) && r>0.1 && g<0.1;
    if(mag)
    {
         gl_FragColor = vec4(
             hsv2rgb(
                 vec3(4.0 * vTextureCoord.y + (-0.02 * time),
                      1.0,
                      1.0)),
             1.0) * r;
    }
    else
    {
        gl_FragColor = col;
    }
}
"]
         #js {"time" #js {"type" "1f" "value" 0.0}})]
    (go
      (loop [frame 0]
        (<! (events/next-frame))
        (set! (.-uniforms.time.value f) (float frame))
        (recur (inc frame))))
    f))

;;
;; Wave scroller
;;
(defn make-wavy []
  (let [f
        (PIXI/AbstractFilter.
         #js [
              "
precision mediump float;
varying vec2 vTextureCoord;
varying vec4 vColor;
uniform sampler2D uSampler;
uniform float time;
// shader mainline
void main( void ) {
    float x=vTextureCoord.x;
    float y=vTextureCoord.y;
    vec4 col = texture2D(uSampler,
                         vec2(x,
                              y - 0.02 * sin(0.1 * time + 10.0 * x))
                         //vec2(x,y)
                         );
    gl_FragColor = col;
}
"]
         #js {"time" #js {"type" "1f" "value" 0.0}})]
    (go
      (loop [frame 0]
        (<! (events/next-frame))
        (set! (.-uniforms.time.value f) (float frame))
        (recur (inc frame))))
    f))
