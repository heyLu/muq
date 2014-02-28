(ns mutomic.test
  (:use [clojure.test :only (deftest is)]
        [mutomic]))

(deftest test-data-matches
  (is (= (data-matches '?a 'b) '{?a b}))
  (is (= (data-matches '?a 3) '{?a 3}))
  (is (= (data-matches 3 3) {}))
  (is (= (data-matches 3 4) nil)))

(deftest test-clause-matches
  (is (= (clause-matches '[?e :name "Joe"] [:joe :name "Joe"])
         '{?e :joe})))

(let [clause1 '[?e :name "Joe"]
      clause2 '[?e :name "Fred"]
      clause3 '[?e :age 10]
      joe [:joe :name "Joe"]
      fred [:fred :name "Fred"]]
  (vector ;bindings-consistent?
    (clause-matches clause1 joe)
    (clause-matches clause3 joe)))

(deftest test-filter-clauses
  (is (= (count (filter #(clause-matches '[?e :name ?n] %) fred-julia-joe))
         3)))

(step-expression-clause '{?e 3} '[(< ?e 4)])
(step-expression-clause '{?e " \t"} '[(clojure.string/blank? ?e)])
(step-expression-clause '{?e "Fred"} '[(#{"Fred" "Julia"} ?e)])
(step-expression-clause '{?e "Fred"} '[({"Fred" "lonely"} ?e) ?state])
(step-expression-clause '{?e :fred} '[({:fred :lonely, :julia :fancy} ?e) ?mood])
(step-expression-clause {'?d1 #inst "2014-02-13" '?d2 #inst "2014-02-27"} '[(> ?d1 ?d2)])

(deftest test-step*
  (let [env '{?e :joe ?n "Joe"}]
    (is (= (step* env '[(= ?n "Joe")] fred-julia-joe)
           (list env)))
    (is (empty? (step* env '[(= ?n "Trudy")] fred-julia-joe)))))

(deftest test-resolve-var*
  (let [friends-clauses '[[?e :likes ?o] [?o :likes ?e]]
        friends (resolve-var* {} friends-clauses {'$ fred-julia-joe})]
    (is (= (count friends) 2))))

(deftest test-query-naive
  (let [friends-with-attrs '[[?e :name ?n]
                             [?e :age ?a]
                             [?e :likes ?o]
                             [?o :likes ?e]]
        friends (query-naive {} friends-with-attrs {'$ fred-julia-joe})]
    (is (= (count friends) 2))))

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
