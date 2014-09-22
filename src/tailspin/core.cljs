(ns tailspin.core
  (:require [cljs.core.async :as async]
            [cljs.reader :as rdr]
            [clojure.set :as set]
            [dep-graph.core :as dep]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(enable-console-print!)

(defn- make-cell []
  {:type :code :name (name (gensym "cell")) :input "nil"})

(defn- keyed-by [keyfn coll]
  (reduce (fn [m item] (assoc m (keyfn item) item)) {} coll))

(def app-state
  (atom {:cells [{:type :code
                  :name "testing"
                  :input "(str \"Hello, \" \"world!\")"}]
         :deps (dep/graph)}))

;; ---------------------------------------------------------------------
;; Evaluation utilities

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

(def ^:private builtins
  {'+ + '- - '* * '/ / '= = '> > '>= >= '< < '<= <= 'apply apply 'dec dec 'inc inc 'str str})

;; ---------------------------------------------------------------------
;; Recalculate cell values after cell update

(defn- calculate-dependencies [code]
  (letfn [(deps* [form]
            (cond (coll? form) (apply set/union (map deps* form))
                  (symbol? form) #{(name form)}
                  :else #{}))]
    (try (set/difference (deps* (rdr/read-string code)) (set (map name (keys builtins))))
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
              (or (builtins sym)
                  (throw (js/Error. (str "Can't resolve symbol '" sym "'"))))))]
    (assoc cell :output
      (try {:value (eval* (rdr/read-string (:input cell)) lookup)}
           (catch js/Error err {:error (.-message err)})))))

(defn- update-cell
  "Given a map of `sheet` data and a map containing keys `:name` (the name of a
   cell to update) and `:input` (the new input string for the updated cell),
   updates the entire sheet to reflect the changes made."
  [sheet {:keys [name input]}]
  (let [graph (:deps sheet)
        get-cell (assoc-in (keyed-by :name (:cells sheet)) [name :input] input)
        ;; we'll use this info to calculate an updated dependency graph below
        new-deps (calculate-dependencies input)
        old-deps (dep/immediate-dependencies graph name)
        +deps (set/difference new-deps old-deps)
        -deps (set/difference old-deps new-deps)
        ;; check for circular dependencies to ensure nothing breaks
        downstream (dep/transitive-dependents graph name)
        circ-dep (first (set/intersection downstream new-deps))
        the-cell (if circ-dep
                   (assoc (get-cell name) :output
                          {:error (str "Circular dependency on cell '" circ-dep "'")})
                   (recalc (get-cell name) get-cell))
        ;; update downstream cells to reflect the changes we made upstream
        updated (->> (map get-cell downstream)
                     (sort-by :name (dep/topo-comparator graph))
                     (reduce (fn [updated cell]
                               (assoc updated (:name cell)
                                      (recalc cell #(or (updated %) (get-cell %)))))
                             {name the-cell}))]
    {:cells
     (reduce (fn [cells cell]
               (conj cells (or (updated (:name cell)) cell)))
             [] (:cells sheet))
     :deps
     (if circ-dep
       (dep/remove-node graph name)
       (reduce #(dep/remove-edge %1 name %2)
               (reduce #(dep/depend %1 name %2) graph +deps) -deps))}))

;; ---------------------------------------------------------------------
;; Render cells to the DOM

(defn- render-result [{:keys [error value]}]
  (dom/div #js {:className (str "output " (if error "failure" "success"))}
    (str "==> " (or error (pr-str value)))))

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
            :update (om/transact! app-state [] #(update-cell % ev))
            :remove (when (> (count (:cells @app-state)) 1)
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
