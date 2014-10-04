(ns tailspin.core
  (:require [cljs.core.async :as async]
            [cljs.reader :as rdr]
            [clojure.set :as set]
            [dep-graph.core :as dep]
            [markdown.core :refer [md->html]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [tailspin.language :as lang]
            [tailspin.ui :as ui])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(defn- make-code-cell []
  {:type :code :name (name (gensym "cell")) :input "nil"})

(defn- make-text-cell []
  {:type :text :name (name (gensym "cell")) :text "Type here..."})

(defn- keyed-by [keyfn coll]
  (reduce (fn [m item] (assoc m (keyfn item) item)) {} coll))

(def app-state
  (atom {:cells [{:type :text
                  :name "intro"
                  :text (apply str
                          ["Welcome to Tailspin, an interactive notebook-style REPL. "
                           "Feel free to edit this text, or the code below. "
                           "You can also press SHIFT+ENTER to insert a new text cell "
                           "or CMD+ENTER to insert a new code cell."])}
                 {:type :code
                  :name "testing"
                  :input "(str \"Hello, \" \"world!\")"}]
         :deps (dep/graph)}))

;; ---------------------------------------------------------------------
;; Recalculate cell values after cell update

(defn- calculate-dependencies [code]
  (letfn [(deps* [form]
            (cond (coll? form) (apply set/union (map deps* form))
                  (symbol? form) #{(name form)}
                  :else #{}))]
    (try (set/difference (deps* (rdr/read-string code)) (set (map name (keys lang/builtins))))
         (catch js/Error _ #{}))))

(defn- recalc
  "Given an updated `cell` and a function `get-cell` that returns the cell data
   for any given cell name, returns a modified copy of `cell` whose `:output`
   reflects the changes made to `:input`."
  [cell get-cell]
  (letfn [(lookup [sym]
            (if-let [dep (get-cell (name sym))]
              (if (contains? (:output dep) :error)
                (throw (js/Error. (str "Cell '" sym "' contains an error")))
                (:value (:output dep)))
              (or (lang/builtins sym)
                  (throw (js/Error. (str "Can't resolve symbol '" sym "'"))))))]
    (assoc cell :output
      (try {:value (lang/eval (rdr/read-string (:input cell)) lookup)}
           (catch js/Error err {:error (.-message err)})))))

(defn- updates [graph name get-cell]
  (->> (map get-cell (dep/transitive-dependents graph name))
       (sort-by :name (dep/topo-comparator graph))
       (reduce (fn [updated cell]
                 (assoc updated (:name cell)
                        (recalc cell #(or (updated %) (get-cell %)))))
               {name (get-cell name)})))

(defn- with-updates [updates cells]
  (mapv #(or (updates (:name %)) %) cells))

;; ---------------------------------------------------------------------
;; Transactionally update the entire sheet in response to user actions

(defn- refresh-cell
  "When the user changes a UI input's value, recalculate dependent cells."
  [{:keys [deps cells]} {:keys [name value]}]
  (let [get-cell (assoc-in (keyed-by :name cells) [name :output :value ::ui/value] value)]
    {:cells (with-updates (updates deps name get-cell) cells) :deps deps}))

(defn- remove-cell
  "When the user deletes a cell, recalculate dependent cells."
  [{:keys [deps cells]} {:keys [name]}]
  (let [get-cell (dissoc (keyed-by :name cells) name)]
    {:cells (->> (remove #(= (:name %) name) cells)
                 (with-updates (updates deps name get-cell)))
     :deps (dep/remove-all deps name)}))

(defn- update-cell
  "When the user changes a cell's code, recalculate dependent cells."
  [{:keys [deps cells]} {:keys [name input]}]
  (let [get-cell (assoc-in (keyed-by :name cells) [name :input] input)
        new-deps (calculate-dependencies input)
        old-deps (dep/immediate-dependencies deps name)
        +deps (set/difference new-deps old-deps)
        -deps (set/difference old-deps new-deps)
        downstream (dep/transitive-dependents deps name)
        circ-dep (first (set/intersection (conj downstream name) new-deps))
        get-cell (if circ-dep
                   (assoc-in get-cell [name :output]
                             {:error (str "Circular dependency on cell '" circ-dep "'")})
                   (update-in get-cell [name] recalc get-cell))]
    {:cells (with-updates (updates deps name get-cell) cells)
     :deps (if circ-dep
             (dep/remove-node deps name)
             (reduce #(dep/remove-edge %1 name %2)
                     (reduce #(dep/depend %1 name %2) deps +deps) -deps))}))

;; ---------------------------------------------------------------------
;; Render cells to the DOM

(defn code-cell [cell owner opts]
  (reify
    om/IWillMount
    (will-mount [_]
      (om/transact! cell [] #(recalc % (constantly nil))))
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
                 :onChange #(async/put! (:event-bus opts)
                              {:op :update :name (:name @cell) :input (.. % -target -value)})
                 :onKeyDown #(when (and (= (.-keyCode %) 8) (= (.. % -target -value) ""))
                               (async/put! (:event-bus opts) {:op :remove :name (:name @cell)}))
                 :value (:input cell)})
          (let [{:keys [error value]} (:output cell)]
            (dom/div #js {:className (str "output " (if error "failure" "success"))}
              (if (ui/spec? value)
                (ui/build value #(async/put! (:event-bus opts)
                                   {:op :refresh :name (:name @cell) :value %}))
                (str "==> " (or error (pr-str value)))))))))))

(defn text-cell [cell owner opts]
  (reify
    om/IRenderState
    (render-state [_ {:keys [editing?]}]
      (dom/div #js {:className "cell text"}
        (dom/div #js {:className "midcol"}
          (if editing?
            (dom/textarea
              #js {:className "input"
                   :cols 66
                   :onBlur #(om/set-state! owner :editing? false)
                   :onChange #(om/update! cell :text (.. % -target -value))
                   :onKeyDown #(when (and (= (.-keyCode %) 8) (= (.. % -target -value) ""))
                                 (async/put! (:event-bus opts) {:op :remove :name (:name @cell)}))
                   :ref "input"
                   :rows (str (inc (int (/ (count (:text cell)) 66))))
                   :value (:text cell)})
            (dom/div
              #js {:className "content"
                   :dangerouslySetInnerHTML #js {:__html (md->html (:text cell))}
                   :onClick #(om/set-state! owner :editing? true)})))))
    om/IDidUpdate
    (did-update [_ _ {was-editing? :editing?}]
      (let [editing? (om/get-state owner :editing?)]
        (when (and editing? (not was-editing?))
          (.focus (om/get-node owner "input")))))))

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
            :refresh (om/transact! app-state [] #(refresh-cell % ev))
            :update (om/transact! app-state [] #(update-cell % ev))
            :remove (when (> (count (:cells @app-state)) 1)
                      (om/transact! app-state [] #(remove-cell % ev))))
          (recur))))
    om/IRenderState
    (render-state [_ {:keys [event-bus]}]
      (apply dom/div
        #js {:className "tailspin sheet"
             :onKeyDown (fn [ev]
                          (cond (and (= (.-keyCode ev) 13) (.-shiftKey ev))
                                (do (.preventDefault ev)
                                    (om/transact! app-state :cells #(conj % (make-text-cell))))
                                (and (= (.-keyCode ev) 13) (.-metaKey ev))
                                (do (.preventDefault ev)
                                    (om/transact! app-state :cells #(conj % (make-code-cell))))))}
        (for [cell (:cells app-state)]
          (case (:type cell)
            :code (om/build code-cell cell {:opts {:event-bus event-bus}})
            :text (om/build text-cell cell {:opts {:event-bus event-bus}})))))))

(om/root sheet app-state
  {:target (.getElementById js/document "app")})
