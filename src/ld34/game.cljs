(ns ld34.game
  (:require [infinitelives.utils.vec2 :as vec2])
)

(def game (atom {}))

(defn init [set-seed-text new-flies]
  (reset! game
          {
           :frame 0
           :seeds 3
           :dollars 0
           :walker {:buttons false
                    :action :walk ;; :walk :plant
                    :action-count 0 ;; when we want to measure how long weve been in an action for
                    ;; if were harvesting, this is the plant
                    :chopping nil
                    :level 0
                    }
           :plants #{}
           :flies #{(new-flies (vec2/scale (vec2/random-unit) 1000))}
           :hippies #{}
           :caravan {:buttons false}

           :max-seeds 1
           :growth-rate 0.002
           :chop-num 5
           :chop-length 60
           :plant-length 400
           :level 1
           :plant-multiplier 1

           :levels {
                    :man
                    {
                     :cost 1000
                     :activate (fn [g]
                                 (let [res
                                       (-> g
                                           (update :dollars - (-> g :levels :man :cost))
                                           (update-in [:levels :man :cost] * 2)
                                           (update :chop-num #(max 1 (dec %)))
                                           (update :chop-length #(max 10 (- % 10)))
                                           (update :plant-length #(max 30 (- % 70)))
                                           (update :level inc))]
                                   (set-seed-text (:seeds res) (:dollars res))

                                   res))}

                    :faster
                    {
                     :cost 5000
                     :activate (fn [g]
                                 (let [res
                                       (-> g
                                           (update :dollars - (-> g :levels :faster :cost))
                                           (update-in [:levels :faster :cost] * 3)
                                           (update :growth-rate * 2)
                                           (update :level inc))]
                                   (set-seed-text (:seeds res) (:dollars res))
                                   res))}

                    :seed
                    {
                     :cost 10000
                     :activate (fn [g]
                                 (let [res
                                       (-> g
                                           (update :dollars - (-> g :levels :seed :cost))
                                           (update-in [:levels :seed :cost] * 3)
                                           (update :max-seeds inc)
                                           (update :plant-multiplier * 2)
                                           (update :level inc))]
                                   (set-seed-text (:seeds res) (:dollars res))
                                   res))}}})
  )
