(ns tailspin.ui
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(defn spec? [value]
  (and (map? value) (::build value)))

(defn build [spec refresh-cb]
  (om/build (::build spec) (dissoc spec ::build ::deref)
            {:opts {:refresh-cb refresh-cb}}))

(defn deref* [spec]
  (if (spec? spec)
    ((or (::deref spec) ::value) spec)
    (throw (js/Error. "May only deref UI elements"))))

(defn panel-view [data owner opts]
  (reify om/IRender
    (render [_]
      (apply dom/div #js {:className "panel"}
        (for [[field-name spec] (::value data)]
          (dom/div #js {:className "row"}
            (dom/p #js {:className "label"} (name field-name))
            (build spec #((:refresh-cb opts)
                          (assoc-in (::value @data) [field-name ::value] %)))))))))

(defn panel [specs]
  {::build panel-view
   ::deref #(zipmap (keys (::value %)) (map deref* (vals (::value %))))
   ::value (into (sorted-map) specs)})

(defn slider-view [data owner opts]
  (reify om/IRender
    (render [_]
      (dom/div nil
        (dom/input
          #js {:min (:min data)
               :max (:max data)
               :onChange #((:refresh-cb opts) (.. % -target -valueAsNumber))
               :step (:step data)
               :type "range"
               :value (::value data)})
        (dom/output
          #js {:style #js {:position "relative" :left "10px"}
               :value (::value data)})))))

(defn slider
  [& {:keys [min max step init] :or {min 0 max 1 step 0.01 init 0.5}}]
  {::build slider-view :min min :max max :step step ::value init})

(defn textfield-view [data owner opts]
  (reify om/IRender
    (render [_]
      (dom/input
        #js {:onChange #((:refresh-cb opts) (.. % -target -value))
             :type "text"
             :value (::value data)}))))

(defn textfield [& {:keys [init] :or {init ""}}]
  {::build textfield-view ::value init})
