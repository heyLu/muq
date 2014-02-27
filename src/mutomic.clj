(ns mutomic
  "A µcroscopic in-memory temporal database.

Trying to understand datomic, mostly."
  (:require [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.data.fressian :as fress]
            [clojure.java.io :as io])
  (:use [clojure.test :only (deftest is)]))

(defn data-matches [a b]
  (if (symbol? a)
    {a b}
    (if (= a b) {} nil)))

(deftest test-data-matches
  (is (= (data-matches 'a 'b) '{a b}))
  (is (= (data-matches 'a 3) '{a 3}))
  (is (= (data-matches 3 3) {}))
  (is (= (data-matches 3 4) nil)))

(defn clause-matches [clause datom]
  (let [[ce ca cv] clause
        [de da dv] datom
        me (data-matches ce de)
        ma (data-matches ca da)
        mv (data-matches cv dv)]
    (if (and me ma mv)
      (into {} (filter map? [me ma mv]))
      nil)))

(deftest test-clause-matches
  (is (= (clause-matches '[?e :name "Joe"] [:joe :name "Joe"])
         '{?e :joe})))

(defn bindings-consistent? [b1 b2]
  (let [common-keys (set/intersection (into #{} (keys b1)) (into #{} (keys b2)))]
    (every? (fn [k]
              (= (b1 k) (b2 k)))
            common-keys)))

(let [clause1 '[?e :name "Joe"]
      clause2 '[?e :name "Fred"]
      clause3 '[?e :age 10]
      joe [:joe :name "Joe"]
      fred [:fred :name "Fred"]]
  (vector ;bindings-consistent?
    (clause-matches clause1 joe)
    (clause-matches clause3 joe)))

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

(deftest test-filter-clauses
  (is (= (count (filter #(clause-matches '[?e :name ?n] %) fred-julia-joe))
         3)))

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

(comment
  (defn merge-if-consistent [b1 b2]
    (if (bindings-consistent? b1 b2)))

  (defn clauses-match [clauses datoms]
    (reduce (fn [bindings clause]
              (let [new-bindings (clause-matches clause datom)]
                (if bindings
                  (if (and new-binding (bindings-consistent? bindings new-binding))
                    (conj bindings new-binding)
                    nil)
                  nil)))
            {}
            clauses))

  (clauses-match '[[?e :name "Joe"]
                   [?e :age 10]
                   [?e :likes :julia]]
                 [:joe :name "Joe"])
  ;=> {?e [[:name "Joe"] [:age 10] [:likes :julia]]}


  '[[?j :name "Joe"]
    [?j :friend ?p]
    [?p :age ?a]
    [(> ?a 10)]]

  ;=> {?e [[:name "Joe"] [:friend ?p]]
  ;    ?p [[:age ?a]]
  ;    ?a [[(> 10)]]}
  ; ?j -> ?p -> ?a
  ; ?j => candidates for ?p
  ; ?p => candidates for ?a
  ; ?a => determines valid ?p and ?j
  )

(defn variable? [v]
  (and (symbol? v) (.startsWith (name v) "?")))

(defn join-vars [clauses]
  (reduce (fn [vars [var count]]
            (if (and (variable? var) (> count 1))
              (conj vars var)
              vars))
          #{}
          (frequencies (apply concat clauses))))

(defn join-var-dependencies [join-vars clauses]
  (reduce (fn [pairs [e & vs]]
            (let [jvs (filter #(contains? join-vars %) vs)]
              (if (and (contains? join-vars e) (seq jvs))
                (reduce #(update-in %1 [e] conj %2) pairs jvs)
                pairs)))
          {}
          clauses))

;(join-var-dependencies (join-vars story-clauses) story-clauses)

(defn root-vars [clauses]
  (filter (fn [var]
            (every? #(not (some #{var} (rest %))) clauses))
          (join-vars clauses)))

;(root-vars story-clauses)

(defn root-var [clauses]
  (or (first (root-vars clauses))
      (let [join-var-deps (join-var-dependencies (join-vars clauses) clauses)]
        (ffirst (sort-by (fn [[var deps]] (- (count deps))) join-var-deps)))))

(root-var '[[?e :likes ?o] [?o :likes ?e]])

(defn clause-dependencies [join-vars clause]
  (let [[e? a? v?] (map #(if (symbol? %) %) clause)
        {js true vs false} (group-by #(contains? join-vars %) (filter variable? clause))]
    [js vs]))

(clause-dependencies '#{?e ?tx ?user} '[?e :story/title ?v ?tx ?added])

; *join vars* are variables that appear more than once?
; only join vars have dependencies
; or rather: dependencies between join vars are special

(defn dependencies [clauses]
  (join-var-dependencies (join-vars clauses) clauses))

(defn clauses-with [var clauses]
  (filter #(some #{var} %) clauses))

(defn join-clauses-with [var clauses]
  (filter #(= (first %) var) clauses))

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

(defn step-expression-clause [env clause]
  (let [[[f & args] & [pattern]] clause
        args (map #(or (env %) %) args)
        fn (cond
            (symbol? f) (or (resolve f) (throw (IllegalArgumentException. (str "Can't resolve '" f "'"))))
            (ifn? f) f
            :else (throw (IllegalArgumentException. (str f " is not a function"))))]
    (if pattern
      (bind-results env pattern (apply fn args))
      (if (apply fn args)
        (list env)
        '()))))

(step-expression-clause '{?e 3} '[(< ?e 4)])
(step-expression-clause '{?e " \t"} '[(str/blank? ?e)])
(step-expression-clause '{?e "Fred"} '[(#{"Fred" "Julia"} ?e)])
(step-expression-clause '{?e "Fred"} '[({"Fred" "lonely"} ?e) ?state])
(step-expression-clause '{?e :fred} '[({:fred :lonely, :julia :fancy} ?e) ?mood])

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

(deftest test-step*
  (let [env '{?e :joe ?n "Joe"}]
    (is (= (step* env '[(= ?n "Joe")] fred-julia-joe)
           (list env)))
    (is (empty? (step* env '[(= ?n "Trudy")] fred-julia-joe)))))

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

; take one clause
; match it against the datoms -> set of matches that bind the var
; for each of those:
;   take one clause and replace the bound vars in it
;   match it against the datoms -> set of matches with possibly new bindings
;   back to 'for each of those ...'

(defn resolve-var* [env clauses datoms]
  (if (seq clauses)
    (if-let [envs (step env (first clauses) datoms)]
      (flatten (map #(resolve-var* % (rest clauses) datoms) envs))
      nil)
    env))

(deftest test-resolve-var*
  (let [friends-clauses '[[?e :likes ?o] [?o :likes ?e]]
        friends (resolve-var* {} friends-clauses fred-julia-joe)]
    (is (= (count friends) 2))))

(defn resolve-var [var-name clauses datoms]
  (let [clauses (clauses-with var-name clauses)]
    (resolve-var* {} clauses datoms)))

(comment
  (resolve-var '?e '[[?e :name ?n] [?e :age ?a]] fred-julia-joe)

  (resolve-var '?o '[[?e :likes ?o] [?o :likes ?e]] fred-julia-joe)

  ; does the wrong thing, e.g doesn't collect :name and :age for ?e, too.
  ; maybe this would make sense if we resolve ?e in a separate step?
  (resolve-var '?o '[[?e :name ?n]
                     [?e :age ?a]
                     [?e :likes ?o]
                     [?o :likes ?e]] fred-julia-joe))

(defn sort-clauses [clauses]
  (let [{expr-clauses true, clauses false} (group-by expression-clause? clauses)]
    (concat clauses expr-clauses)))

(defn query-naive [env clauses datoms]
  (resolve-var* env (sort-clauses clauses) datoms))

(deftest test-query-naive
  (let [friends-with-attrs '[[?e :name ?n]
                             [?e :age ?a]
                             [?e :likes ?o]
                             [?o :likes ?e]]
        friends (query-naive friends-with-attrs fred-julia-joe)]
    (is (= (count friends) 2))))

(defn normalize-query [query]
  (let [default {:in '[$]}]
    (if (map? query)
      (into default query)
      (into default
            (map (fn [[k v]]
                   [(first k) (vec v)])
                 (partition 2 (partition-by keyword? query)))))))

; possibilities for in-vars:
; * provide an initial env, for vars in a collection we need to execute the query (count coll) times (oops)
; * unify "artificially" by inserting conditional expression clauses:
;     :in $ [?var ...] => [(#{?var ...})]

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

(deftest test-q
  (let [map-query '{:find [?e]
                    :where [[?e :name ?n]
                            [?e :age ?a]]}
        list-query '[:find ?e
                     :where [?e :name ?n]
                     [?e :age ?a]]]
    (is (= (q map-query fred-julia-joe)
           (q list-query fred-julia-joe)))
    (is (= (q map-query fred-julia-joe-index)
           (q list-query fred-julia-joe-index)))))

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

(defn movie-data []
  (let [movies-url "https://raw.github.com/jonase/learndatalogtoday/master/resources/db/data.edn"]
    (edn/read {:readers {'db/id (fn [[_ n]] (next-id n))}}
              (java.io.PushbackReader. (io/reader movies-url)))))

(comment
  (def story-clauses
    '[[?e :story/title ?v ?tx ?added]
      [?e :story/url ?url]
      [?tx :source/user ?user]
      [?tx :db/txInstant ?inst]
      [?user :user/email ?email]])

  ; ?e could depend on ?tx, but doesn't. order doesn't matter, so that can't determine it
  ; either.
  ; (result variables don't matter. removing some doesn't change the "shape" of the result.)
  ; maybe knowledge of eav, could help? because we know that a `v` can be a reference
  ; which might indicate that `e`s are more likely be non-dependent?
  ; so maybe it's about vars that occur in the `e` position, but not in the `v` position?

  (dependencies story-clauses)

  ;=> {?e [?v ?tx ?added ?url]
  ;    ?tx [?user ?inst]
  ;    ?user [?email]}
  ;
  ; variables should only appear as keys if they occur more than once?
  )
