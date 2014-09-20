(ns tailspin.core
  (:require [cljs.reader :as rdr]
            [clojure.walk :as walk]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom {:cells [{:type :code
                  :name "testing"
                  :input "(str \"Hello, \" \"world!\")"
                  :output {:value "Hello, world!"}}]}))

(defn- simple-eval [form]
  (let [smap {'+ + '- - '* * '/ / 'apply apply 'str str}
        form' (walk/prewalk-replace smap form)]
    (apply (first form') (rest form'))))

(defn- handle-change [cell ev]
  (let [input (.. ev -target -value)]
    (om/update! cell :input input)
    (try (let [form (rdr/read-string input)
               result (simple-eval form)]
           (om/update! cell :output {:value result}))
         (catch js/Error err
           (om/update! cell :output {:error (.-message err)})))))

(defn- render-result [{:keys [error value]}]
  (dom/div #js {:className (str "output " (if error "failure" "success"))}
    (str "==> " (or error (pr-str value)))))

(defn code-cell [cell owner]
  (reify om/IRender
    (render [_]
      (dom/div #js {:className "cell code"}
        (dom/p #js {:className "name"} (:name cell))
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
