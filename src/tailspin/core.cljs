(ns tailspin.core
  (:require [cljs.core.async :as async]
            [cljs.reader :as rdr]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(defn- make-cell []
  {:type :code :name (name (gensym "cell")) :input "nil"})

(def app-state
  (atom {:cells [{:type :code
                  :name "testing"
                  :input "(str \"Hello, \" \"world!\")"}]}))

(defn- eval* [form resolve]
  (let [eval* #(eval* % resolve)]
    (condp apply [form]
      seq? (if (empty? form)
             () (apply (eval* (first form)) (map eval* (rest form))))
      vector? (mapv eval* form)
      set? (set (map eval* form))
      map? (into {} (map (fn [[k v]] [(eval* k) (eval* v)]) form))
      symbol? (or (resolve form) (throw (js/Error. (str "Can't resolve symbol '" form "'"))))
      form)))

(defn- shitty-eval [code]
  (let [smap {'+ + '- - '* * '/ / '= = '> > '>= >= '< < '<= <=
              'apply apply 'dec dec 'inc inc 'str str}]
    (try {:value (eval* (rdr/read-string code) smap)}
         (catch js/Error err {:error (.-message err)}))))

(defn- handle-change [cell ev]
  (let [input (.. ev -target -value)]
    (om/transact! cell []
      #(merge % {:input input :output (shitty-eval input)}))))

(defn- render-result [{:keys [error value]}]
  (dom/div #js {:className (str "output " (if error "failure" "success"))}
    (str "==> " (or error (pr-str value)))))

(defn code-cell [cell owner opts]
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
                 :onKeyDown #(when (and (= (.-keyCode %) 8) (= (.. % -target -value) ""))
                               (async/put! (:event-bus opts) {:op :remove :name (:name @cell)}))
                 :value (:input cell)})
          (render-result (:output cell)))))))

(defn sheet [app-state owner]
  (reify
    om/IInitState
    (init-state [_]
      {:event-bus (async/chan)})
    om/IWillMount
    (will-mount [_]
      (go-loop []
        (let [ev (<! (om/get-state owner :event-bus))]
          (case (:op ev)
            :remove
            (when (> (count (:cells @app-state)) 1)
              (om/transact! app-state :cells
                #(filterv (fn [cell] (not= (:name cell) (:name ev))) %))))
          (recur))))
    om/IRenderState
    (render-state [_ {:keys [event-bus]}]
      (apply dom/div
        #js {:className "tailspin sheet"
             :onKeyDown (fn [ev]
                          (when (and (= (.-keyCode ev) 13) (.-shiftKey ev))
                            (.preventDefault ev)
                            (om/transact! app-state :cells #(conj % (make-cell)))))}
        (for [cell (:cells app-state)]
          (case (:type cell)
            :code (om/build code-cell cell {:opts {:event-bus event-bus}})))))))

(om/root sheet app-state
  {:target (.getElementById js/document "app")})
