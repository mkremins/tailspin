(ns tailspin.ui
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(defn spec? [value]
  (and (map? value) (::component value)))

(defn slider-view [data owner opts]
  (reify om/IRender
    (render [_]
      (dom/div #js {:style #js {:margin-top "2px"}}
        (dom/input
          #js {:min (:min data)
               :max (:max data)
               :onChange #((:refresh-cb opts) (.. % -target -valueAsNumber))
               :step (:step data)
               :type "range"
               :value (:value data)})
        (dom/output
          #js {:style #js {:position "relative" :left "10px" :top "-2px"}
               :value (:value data)})))))

(defn slider
  [& {:keys [min max step init] :or {min 0 max 1 step 0.01 init 0.5}}]
  {::component slider-view :min min :max max :step step :value init})

(defn build [spec refresh-cb]
  (om/build (::component spec) (dissoc spec ::component)
            {:opts {:refresh-cb refresh-cb}}))
