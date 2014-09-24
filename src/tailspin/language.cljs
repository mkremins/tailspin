(ns tailspin.language
  "Implements a strictly limited subset of the Clojure language."
  (:refer-clojure :exclude [eval]))

(def builtins
  {'+ +, '- -, '* *, '/ /, '< <, '<= <=, '> >, '>= >=, '= =, '== ==, 'aget aget, 'apply apply,
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
   'vector? vector?, 'vec vec, 'zipmap zipmap})

(defn eval [form resolve]
  (let [eval* #(eval % resolve)]
    (condp apply [form]
      seq? (if (empty? form)
             () (apply (eval* (first form)) (map eval* (rest form))))
      vector? (mapv eval* form)
      set? (set (map eval* form))
      map? (into {} (map (fn [[k v]] [(eval* k) (eval* v)]) form))
      symbol? (resolve form)
      form)))
