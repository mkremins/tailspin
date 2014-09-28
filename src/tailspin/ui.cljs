(ns tailspin.ui
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(defn slider
  [& {:keys [min max step init]
      :or {min 0 max 1 step 0.01 init 0.5}}]
  {::type :slider :min min :max max :step step :value init})

(defn slider-view [data owner opts]
  (reify om/IRender
    (render [_]
      (dom/input
        #js {:min (:min data)
             :max (:max data)
             :onChange #((:refresh-cb opts) (js/parseFloat (.. % -target -value)))
             :step (:step data)
             :type "range"
             :value (:value data)}))))
