(ns mutomic
  "A µcroscopic in-memory temporal database.

Trying to understand datomic, mostly."
  (:require [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.data.fressian :as fress]
            [clojure.java.io :as io]))

(defn data-matches [a b]
  (if (symbol? a)
    {a b}
    (if (= a b) {} nil)))

(defn clause-matches [clause datom]
  (let [[ce ca cv] clause
        [de da dv] datom
        me (data-matches ce de)
        ma (data-matches ca da)
        mv (data-matches cv dv)]
    (if (and me ma mv)
      (into {} (filter map? [me ma mv]))
      nil)))

(defn bindings-consistent? [b1 b2]
  (let [common-keys (set/intersection (into #{} (keys b1)) (into #{} (keys b2)))]
    (every? (fn [k]
              (= (b1 k) (b2 k)))
            common-keys)))

(def fred-julia-joe
  [[:fred :name "Fred"]
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

(defrecord Datum [e a v t]
  clojure.lang.Indexed
  (nth [_ i] (case i 0 e 1 a 2 v 3 t
               (throw (IndexOutOfBoundsException.))))
  (nth [this i default]
    (if (<= 0 i 3)
      (nth this i)
      default)))

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

(defn save! [f idx]
  (with-open [w (fress/create-writer (io/output-stream f))]
    (fress/write-object w idx)))

;(save! (java.io.File. "fjj.idx.fsn") fred-julia-joe-index)

(defn load! [f]
  (with-open [r (fress/create-reader (io/input-stream f))]
    (fress/read-object r)))

;(load! "fjj.idx.fsn")

(defn variable? [v]
  (and (symbol? v) (.startsWith (name v) "?")))

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
        :else (throw (IllegalArgumentException. (str "Unsupported pattern " pattern))))))

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
           (or (> c 0) (= c 0))))})

(defn step-expression-clause [env clause]
  (let [[[f & args] & [pattern]] clause
        args (map #(or (env %) %) args)
        fn (cond
            (symbol? f) (or (resolve-internal f) (resolve f) (throw (IllegalArgumentException. (str "Can't resolve '" f "'"))))
            (ifn? f) f
            :else (throw (IllegalArgumentException. (str f " is not a function"))))]
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

(defn step [env clause datoms]
  (if (map? datoms)
    (step-index env clause datoms)
    (step* env clause datoms)))

(into #{} (step {} '[?e :likes ?o] fred-julia-joe))

(into #{} (step {} '[?e :likes ?o] fred-julia-joe-index))

(defn resolve-var* [env clauses datoms]
  (if (seq clauses)
    (if-let [envs (step env (first clauses) datoms)]
      (flatten (map #(resolve-var* % (rest clauses) datoms) envs))
      nil)
    env))

(defn sort-clauses [clauses]
  (let [{expr-clauses true, clauses false} (group-by expression-clause? clauses)]
    (concat clauses expr-clauses)))

(defn query-naive [env clauses datoms]
  (resolve-var* env (sort-clauses clauses) datoms))

(defn normalize-query [query]
  (let [default {:in '[$]}]
    (if (map? query)
      (into default query)
      (into default
            (map (fn [[k v]]
                   [(first k) (vec v)])
                 (partition 2 (partition-by keyword? query)))))))

(defn db? [v]
  (and (symbol? v) (.startsWith (name v) "$")))

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

(defn rel-clauses [{in-vars :in} & inputs]
  (map (fn [[[[a b]] input]]
         (vector (list (into {} input) a) b))
       (filter #(relation-binding? (first %))
               (map vector in-vars inputs))))

(defn initial-environment [{in-vars :in} & inputs]
  (into {}
        (mapcat (fn [var input]
                  (cond
                   (db? var) [[var input]]
                   (variable? var) [[var input]]
                   (tuple-binding? var) (map vector var input)))
                in-vars
                inputs)))

(defn q [query & inputs]
  (let [{vars :find, clauses :where, in-vars :in :as query} (normalize-query query)
        initial-env (apply initial-environment query inputs)
        clauses (concat clauses
                        (apply coll-clauses query inputs)
                        (apply rel-clauses query inputs))]
    (into #{}
          (map (fn [env]
                 (mapv env vars))
               (query-naive initial-env clauses (first inputs))))))

(defn random-name []
  (let [alphabet "abcdefghijklmnopqrstuvwxyz"
        vowels "aeiou"]
    (apply str (concat [(.toUpperCase (str (rand-nth alphabet)))]
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
  (edn/read {:readers {'db/id (fn [[_ n]] (if n (next-id n) (next-id)))}}
            (java.io.PushbackReader. (io/reader f))))

(defn movie-data []
  (read-edn "https://raw.github.com/jonase/learndatalogtoday/master/resources/db/data.edn"))
