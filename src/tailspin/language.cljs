(ns tailspin.language
  "Implements a strictly limited subset of the Clojure language."
  (:refer-clojure :exclude [eval])
  (:require [tailspin.ui :as ui]))

(def ^:dynamic *max-recur-depth* 256)

(def builtins
  { ;; clojure.core pure functions
   '+ +, '- -, '* *, '/ /, '< <, '<= <=, '> >, '>= >=, '= =, '== ==, 'aget aget, 'apply apply,
   'aset aset, 'assoc assoc, 'assoc-in assoc-in, 'comp comp, 'complement complement,
   'concat concat, 'conj conj, 'cons cons, 'constantly constantly, 'contains? contains?,
   'count count, 'cycle cycle, 'dec dec, 'disj disj, 'dissoc dissoc, 'distinct distinct,
   'distinct? distinct?, 'drop drop, 'drop-last drop-last, 'drop-while drop-while, 'empty empty,
   'empty? empty?, 'every? every?, 'filter filter, 'filterv filterv, 'find find, 'first first,
   'ffirst ffirst, 'fnil fnil, 'get get, 'get-in get-in, 'group-by group-by, 'hash hash,
   'hash-map hash-map, 'hash-set hash-set, 'identity identity, 'inc inc, 'interleave interleave,
   'interpose interpose, 'into into, 'iterate iterate, 'juxt juxt, 'key key, 'keys keys,
   'keyword keyword, 'last last, 'list list, 'list* list*, 'map map, 'map? map?, 'mapcat mapcat,
   'mapv mapv, 'map-indexed map-indexed, 'max max, 'merge merge, 'merge-with merge-with, 'min min,
   'mod mod, 'name name, 'namespace namespace, 'next next, 'nil? nil?, 'not not, 'not= not=,
   'not-any? not-any?, 'nth nth, 'nthnext nthnext, 'partial partial, 'partition partition,
   'partition-all partition-all, 'partition-by partition-by, 'peek peek, 'pop pop, 'quot quot,
   'range range, 'reduce reduce, 'reduce-kv reduce-kv, 'rem rem, 'remove remove, 'repeat repeat,
   'replace replace, 'rest rest, 'reverse reverse, 'rseq rseq, 'second second, 'seq seq,
   'seq? seq?, 'set set, 'set? set?, 'select-keys select-keys, 'some some, 'some? some?,
   'sort sort, 'sort-by sort-by, 'split-at split-at, 'split-with split-with, 'str str, 'subs subs,
   'subvec subvec, 'symbol symbol, 'symbol? symbol?, 'take take, 'take-last take-last,
   'take-while take-while, 'update-in update-in, 'val val, 'vals vals, 'vector vector,
   'vector? vector?, 'vec vec, 'zipmap zipmap,
    ;; Tailspin UI elements
   'deref ui/deref*, 'panel ui/panel, 'slider ui/slider, 'textfield ui/textfield})

(declare eval)

(defmulti eval-special (fn [form _] (first form)))

(deftype Function [params body resolve])

(defn- apply* [f args]
  (if (instance? Function f)
    (let [args (zipmap (.-params f) args)]
      (eval (.-body f) #(if (contains? args %) (args %) ((.-resolve f) %))))
    (apply f args)))

(defmethod eval-special 'fn* [[_ params body] resolve]
  (Function. params body resolve))

(defmethod eval-special 'if [[_ test then else] resolve]
  (eval (if (eval test resolve) then else) resolve))

(defmethod eval-special 'let* [[_ bvec body] resolve]
  (loop [bpairs (partition 2 bvec)
         resolve* resolve]
    (if-let [[bsym bval] (first bpairs)]
      (recur (rest bpairs)
             #(if (= % bsym) (eval bval resolve*) (resolve* %)))
      (eval body resolve*))))

(deftype RecurThunk [args])

(defmethod eval-special 'loop* [[_ bvec body] resolve]
  (let [bpairs (partition 2 bvec)
        bsyms (map first bpairs)]
    (loop [bpairs bpairs
           resolve* resolve]
      (if-let [[bsym bval] (first bpairs)]
        (recur (rest bpairs)
               #(if (= % bsym) (eval bval resolve*) (resolve* %)))
        (loop [resolve* resolve*
               recur-depth 0]
          (let [ret (eval body resolve*)]
            (if (instance? RecurThunk ret)
              (if (> recur-depth *max-recur-depth*)
                (throw (js/Error. (str "Maximum recur depth (" *max-recur-depth* ") exceeded")))
                (let [loop-bindings (zipmap bsyms (.-args ret))]
                  (recur #(if (contains? loop-bindings %) (loop-bindings %) (resolve %))
                         (inc recur-depth))))
              ret)))))))

(defmethod eval-special 'quote [[_ arg] _] arg)

(defmethod eval-special 'recur [[_ & args] resolve]
  (RecurThunk. (map #(eval % resolve) args)))

(defn eval [form resolve]
  (let [eval* #(eval % resolve)]
    (condp apply [form]
      seq? (if-let [eval-special* (get-method eval-special (first form))]
             (eval-special* form resolve)
             (if (empty? form) () (apply* (eval* (first form)) (map eval* (rest form)))))
      vector? (mapv eval* form)
      set? (set (map eval* form))
      map? (into {} (map (fn [[k v]] [(eval* k) (eval* v)]) form))
      symbol? (resolve form)
      form)))
