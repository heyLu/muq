(ns mutomic
  "A µcroscopic in-memory temporal database.

Trying to understand datomic, mostly."
  (:require [clojure.set :as set]
            [mutomic.platform :as p]))

(defrecord Datum [e a v t])

(defn db? [v]
  (and (symbol? v) (p/starts-with (name v) "$")))

(defn variable? [v]
  (and (symbol? v) (p/starts-with (name v) "?")))

(defn wildcard? [v]
  (= '_ v))

(defn data-matches [a b]
  (cond
   (variable? a) {a b}
   (wildcard? a) {}
   (= a b) {}
   :else nil))

(defn clause-matches [clause datom]
  (let [ms (map data-matches clause (if (instance? Datum datom) (vals datom) datom))]
    (if (every? identity ms)
      (into {} ms)
      nil)))

(defn bindings-consistent? [b1 b2]
  (let [common-keys (set/intersection (into #{} (keys b1)) (into #{} (keys b2)))]
    (every? (fn [k]
              (= (b1 k) (b2 k)))
            common-keys)))

(def fred-julia-joe
  [[:fred :name "Fred"]
   [:fred :is "awkward"]
   [:julia :name "Julia"]
   [:julia :is "awesome"]
   [:julia :is "fun"]
   [:julia :likes :joe]
   [:fred :age 13]
   [:julia :age 10]
   [:joe :age 7]
   [:joe :likes :fred]
   [:joe :likes :julia]
   [:joe :likes :flowers]
   [:joe :name "Joe"]])

(defn index [idx datom]
  (let [[e a v t] datom
        t 0
        datom (Datum. e a v t)]
    (-> idx
        (assoc-in (into [:eavt] [e a v t]) datom)
        (assoc-in (into [:aevt] [a e v t]) datom))))

(defn index-many [idx datoms]
  (reduce index idx datoms))

(def fred-julia-joe-index
  (index-many nil fred-julia-joe))

(defn datoms [idx idx-name & components]
  (get-in idx (into [] (cons idx-name components))))

;(datoms fred-julia-joe-index :eavt)

(defn flatten-index [idx]
  (if (instance? Datum idx) [idx] (mapcat flatten-index (vals idx))))

(defn entity [idx eid]
  (into {:db/id eid}
        (map (fn [[k vt]]
               [k (-> (flatten-index vt) first :v)])
             (datoms idx :eavt eid))))

(defn replace-vars [env datom]
  (mapv #(or (env %) %) datom))

(defn expression-clause? [clause]
  (list? (first clause)))

(defn tuple-binding? [v]
  (and (vector? v) (every? variable? v)))

(defn merge-if-consistent [env new-env]
  (if (bindings-consistent? env new-env)
    (conj env new-env)
    nil))

(defn bind-results [env pattern result]
  (map #(merge-if-consistent env %)
       (cond
        (variable? pattern) (if-not (nil? result)
                              (list (assoc env pattern result))
                              '())
        (tuple-binding? pattern) (if-not (some nil? result)
                                   (into env (map vector pattern result))
                                   '())
        :else (p/illegal-argument (str "Unsupported pattern " pattern)))))

(def resolve-internal
  {'< (fn [o1 o2]
        (< (compare o1 o2) 0))
   '<= (fn [o1 o2]
         (let [c (compare o1 o2)]
           (or (< c 0) (= c 0))))
   '> (fn [o1 o2]
        (> (compare o1 o2) 0))
   '>= (fn [o1 o2]
        (let [c (compare o1 o2)]
           (or (> c 0) (= c 0))))
   'not= not=})

(defn step-expression-clause [env clause]
  (let [[[f & args] & [pattern]] clause
        args (map #(or (env %) %) args)
        fn (cond
            (symbol? f) (or (resolve-internal f) (p/resolve f) (p/illegal-argument (str "Can't resolve '" f "'")))
            (ifn? f) f
            :else (p/illegal-argument (str f " is not a function")))]
    (if pattern
      (bind-results env pattern (apply fn args))
      (if (apply fn args)
        (list env)
        '()))))

(defn step-binding [env clause datom]
  (if-let [new-env (clause-matches (replace-vars env clause) datom)]
    (if (bindings-consistent? env new-env)
      (conj env new-env)
      nil)
    nil))

(defn step* [env clause datoms]
  (filter identity
          (if (expression-clause? clause)
            (step-expression-clause env clause)
            (map (fn [datom]
                   (step-binding env clause datom))
                 datoms))))

(defn step-index [env clause idx]
  (let [[e a v] (replace-vars env clause)
        [e? a? v?] (map (comp not variable?) [e a v])
        t 0
        datoms (cond
                (and e? a? v?) (datoms idx :eavt e a v)
                (and e? a?) (datoms idx :eavt e a)
                e? (datoms idx :eavt e)
                a? (datoms idx :aevt a)
                :else (do
                        (println "Warning: querying against the whole index")
                        (datoms idx :eavt)))]
    (step* env clause (flatten-index datoms))))

(defn step [env clause dbs & _]
  (let [db (first clause)
        [clause datoms] (if (db? db)
                          [(subvec clause 1) (dbs db)]
                          [clause (dbs '$)])]
    (if (map? datoms)
      (step-index env clause datoms)
      (step* env clause datoms))))

(into #{} (step {} '[?e :likes ?o] {'$ fred-julia-joe}))

(into #{} (step {} '[?e :likes ?o] {'$ fred-julia-joe-index}))

(defn rule-name [rule-def]
  (ffirst rule-def))

(defn remap-keys [m key-map]
  (let [m (select-keys m (keys key-map))]
    (into {} (map vector (map key-map (keys m)) (vals m)))))

(defn reduce-until [pred f val coll]
  (if (seq coll)
    (let [r (f val (first coll))]
      (if (pred val r)
        r
        (recur pred f r (rest coll))))
    val))

(declare resolve-var*)

; rules are like a separate query with an initial env given?
; what does "or" mean? do we need to keep track whether one rule
; failed earlier and then retry with the potential alternatives?
; gets much trickier because the "rule env" is different from the
; query env, e.g. bindings from rule env must be translated from
; and to the query end.
(defn resolve-rule [env rule-call dbs state]
  (let [[name & params] rule-call
        defs (filter #(= (rule-name %) name) (dbs '%))
        [_ & rparams] (ffirst defs)
        {rule-env false rule->query true} (group-by #(-> % second variable?) (map vector rparams (replace-vars env params)))
        [rule-env rule->query] (map #(into {} %) [rule-env rule->query])]
    (if (contains? @state [name (replace-vars env params)])
      #{}
      (do
        (swap! state conj [name (replace-vars env params)])
        (reduce into #{}
                (map (fn [rdef]
                       (let [[_ & clauses] rdef
                             results (resolve-var* rule-env clauses dbs state)]
                         (filter identity
                                 (map #(if (map? %) (merge-if-consistent env (remap-keys % rule->query))) results))))
                     defs))))))

(defn rule? [clause]
  (list? clause))

(defn resolve-var* [env clauses dbs state]
  (flatten
   (if (seq clauses)
     (let [clause (first clauses)
           step-fn (if (rule? clause) resolve-rule step)]
       (if-let [envs (step-fn env clause dbs state)]
         (map #(resolve-var* % (rest clauses) dbs state) envs)
         nil))
     (list env))))

(defn sort-clauses [clauses]
  (let [{expr-clauses true, clauses false} (group-by expression-clause? clauses)]
    (concat clauses expr-clauses)))

(defn query-naive [env clauses dbs]
  (resolve-var* env (sort-clauses clauses) dbs (atom #{})))

(defn normalize-query [query]
  (let [default {:in '[$]}]
    (if (map? query)
      (into default query)
      (into default
            (map (fn [[k v]]
                   [(first k) (vec v)])
                 (partition 2 (partition-by keyword? query)))))))

(defn collection-binding? [v]
  (and (vector? v) (variable? (first v)) (= (second v) '...)))

(defn coll-clauses [{in-vars :in} & inputs]
  (map (fn [[var input]]
         ;`[(~(set input) ~(first var))]
         (vector (list (set input) (first var))))
       (filter #(collection-binding? (first %))
               (map vector in-vars inputs))))

(defn relation-binding? [v]
  (and (vector? v) (vector? (first v))))

(defn relation [[[[var-a var-b]] input]]
  (let [db (gensym "$rel")
        rel (gensym "rel")
        env {db (map (fn [[in-a in-b]]
                       [in-a rel in-b])
                     input)}
        clause [db var-a rel var-b]]
    [env clause]))

(defn relations [{in-vars :in} & inputs]
  (let [rels (filter #(relation-binding? (first %))
                     (map vector in-vars inputs))]
    (reduce (fn [[envs clauses] [env clause]]
              [(conj envs env) (conj clauses clause)])
            [{} []]
            (map relation rels))))

(defn initial-environment [{in-vars :in} & inputs]
  (into {}
        (mapcat (fn [var input]
                  (cond
                   (variable? var) [[var input]]
                   (tuple-binding? var) (map vector var input)))
                in-vars
                inputs)))

(defn q [query & inputs]
  (let [{vars :find, clauses :where, in-vars :in :as query} (normalize-query query)
        initial-env (apply initial-environment query inputs)
        dbs (into {} (filter (fn [[v _]]
                               (or (db? v) (= '% v)))
                             (map vector in-vars inputs)))
        [rel-envs rel-clauses] (apply relations query inputs)
        clauses (concat clauses
                        (apply coll-clauses query inputs)
                        rel-clauses)]
    (into #{}
          (map (fn [env]
                 (mapv env vars))
               (query-naive initial-env clauses (conj dbs rel-envs))))))

(defn random-name []
  (let [alphabet "abcdefghijklmnopqrstuvwxyz"
        vowels "aeiou"]
    (apply str (concat [(p/to-upper-case (str (rand-nth alphabet)))]
                       (map (fn [_]
                              (if (< (rand) 0.3)
                                (rand-nth vowels)
                                (rand-nth alphabet)))
                            (range (+ (rand-int 7) 3)))))))

(defn random-people [n]
  (reduce (fn [ps facts]
            (apply conj ps facts))
          []
   (map (fn [i]
          (let [id (symbol (str "p" i))]
            [[id :name (random-name)]
             [id :age (rand-int 100)]]))
        (range n))))

(defn rand-pred [pred rand-fn & args]
  (let [n (apply rand-fn args)]
    (if (pred n)
      n
      (recur pred rand-fn args))))

(defn random-friends [people n]
  (let [c (count people)]
    (for [_ (range n)]
      (let [p1 (rand-pred odd? rand-int c)
            p2 (rand-pred odd? rand-int c)]
        [p1 :likes p2]))))

(defn random-society [num-people num-friends]
  (let [people (random-people num-people)]
    (concat people (random-friends people num-friends))))

(defn tiny-benchmark
  "benchmark non-indexed data vs indexed data.

note that the calculation gets quite expensive when both numbers are high, but
stay low when only the number of people is high.

that means our index kind of works and that the cross-product is expensive.

don't try the non-indexed version with high numbers unless you have a lot of time
to spare."
  []
  (let [s1 (random-society 1000000 10000)
        s1i (index-many nil s1)
        query '{:find [?e ?o]
                :where [[?e :likes ?o]
                        [?o :likes ?e]]}]
    (prn "go!")
    ;(time (doall (q query s1)))
    (time (doall (q query s1i)))))

(defn next-id-maker []
  (let [id-info (atom {:next-id 0
                       :mapped-ids {}})]
    (fn [& n]
      (let [new-info (swap! id-info
                            (fn [{:keys [next-id mapped-ids] :as id-info}]
                              (if n
                                (if (contains? mapped-ids n)
                                  id-info
                                  {:next-id (inc next-id)
                                   :mapped-ids (assoc mapped-ids n next-id)})
                                (assoc id-info :next-id (inc next-id)))))]
        (if n
          (get-in new-info [:mapped-ids n])
          (dec (:next-id new-info)))))))

(def next-id (next-id-maker))

(defn expand-tx-datum [tx-datum]
  (let [id (:db/id tx-datum)]
    (mapcat (fn [[k v]]
              (if (coll? v)
                (map #(vector id k %) v)
                [[id k v]]))
            (dissoc tx-datum :db/id))))

(defn expand-tx-data [tx-data]
  (mapcat (fn [tx-datum]
            (if (map? tx-datum)
              (expand-tx-datum tx-datum)
              [tx-datum]))
          tx-data))

(defn read-edn [f]
  (p/read-edn {'db/id (fn [[_ n]] (if n (next-id n) (next-id)))}
              f))

(defn movie-data []
  (read-edn "https://raw.github.com/jonase/learndatalogtoday/master/resources/db/data.edn"))
