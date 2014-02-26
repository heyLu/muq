(ns mutomic
  "A µcroscopic in-memory temporal database in memory.

Trying to understand datomic, mostly."
  (:require [clojure.set :as set])
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

(defn step [env clause datoms]
  (filter identity
          (map (fn [datom]
                 (if-let [new-env (clause-matches (replace-vars env clause) datom)]
                   (if (bindings-consistent? env new-env)
                     (conj env new-env)
                     nil)
                   nil))
               datoms)))

(step {} '[?e :likes ?o] fred-julia-joe)

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

(defn query-naive [clauses datoms]
  (resolve-var* {} clauses datoms))

(deftest test-query-naive
  (let [friends-with-attrs '[[?e :name ?n]
                             [?e :age ?a]
                             [?e :likes ?o]
                             [?o :likes ?e]]
        friends (query-naive friends-with-attrs fred-julia-joe)]
    (is (= (count friends) 2))))

(defn normalize-query [query]
  (if (map? query)
    query
    (into {} (map (fn [[k v]]
                      [(first k) (vec v)])
                    (partition 2 (partition-by keyword? query))))))

(defn q [query db]
  (let [{clauses :where} (normalize-query query)]
    (query-naive clauses db)))

(deftest test-q
  (let [map-query '{:find [?e]
                    :where [[?e :name ?n]
                            [?e :age ?a]]}
        list-query '[:find ?e
                     :where [?e :name ?n]
                     [?e :age ?a]]]
    (is (= (q map-query fred-julia-joe)
           (q list-query fred-julia-joe)))))

(defrecord Datum [e a v t])

(defn index [idx datom]
  (let [[e a v t] datom
        datom (Datum. e a v t)]
    (-> idx
        (assoc-in (into [:eavt] [e a v t]) datom)
        (assoc-in (into [:aevt] [a e v t]) datom))))

(defn index-many [idx datoms]
  (reduce index idx datoms))

(def fred-julia-joe-index
  (index-many nil (map #(conj (vec %) 0) fred-julia-joe)))

(defn datoms [idx idx-name & components]
  (get-in idx (into [] (cons idx-name components))))

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
