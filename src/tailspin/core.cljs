(ns tailspin.core
  (:require [cljs.reader :as rdr]
            [clojure.walk :as walk]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom {:cells [{:type :code
                  :name "testing"
                  :input "(str \"Hello, \" \"world!\")"}]}))

(defn- shitty-eval [code]
  (let [smap {'+ + '- - '* * '/ / 'apply apply 'str str}]
    (try (let [form (->> (rdr/read-string code) (walk/prewalk-replace smap))]
           {:value (apply (first form) (rest form))})
         (catch js/Error err
           {:error (.-message err)}))))

(defn- handle-change [cell ev]
  (let [input (.. ev -target -value)]
    (om/transact! cell []
      #(merge % {:input input :output (shitty-eval input)}))))

(defn- render-result [{:keys [error value]}]
  (dom/div #js {:className (str "output " (if error "failure" "success"))}
    (str "==> " (or error (pr-str value)))))

(defn code-cell [cell owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (om/update! cell :output (shitty-eval (:input cell))))
    om/IRender
    (render [_]
      (dom/div #js {:className "cell code"}
        (dom/input
          #js {:className "name"
               :onChange #(om/update! cell :name (.. % -target -value))
               :type "text"
               :value (:name cell)})
        (dom/div #js {:className "midcol"}
          (dom/textarea
            #js {:className "input"
                 :onChange (partial handle-change cell)
                 :value (:input cell)})
          (render-result (:output cell)))))))

(defn sheet [app-state owner]
  (reify om/IRender
    (render [_]
      (apply dom/div #js {:className "tailspin sheet"}
        (for [cell (:cells app-state)]
          (case (:type cell)
            :code (om/build code-cell cell)))))))

(om/root sheet app-state
  {:target (.getElementById js/document "app")})
