(ns learn-datalog-today
  "Queries from the Learn Datalog Today tutorial.

The original tutorial is aviallable at <http://www.learndatalogtoday.org>."
  (:require [clojure.set :as set]
            [datomic.api :as d]
            [mutomic :as mu]))

(let [base-url "https://raw.github.com/jonase/learndatalogtoday/master/resources/db/"
      movie-schema (mu/expand-tx-data (mu/read-edn (str base-url "schema.edn")))
      movie-data (mu/expand-tx-data (mu/read-edn (str base-url "data.edn")))]
  (def movies (concat movie-schema movie-data))
  (def movies-db (mu/index-many nil movies)))

(defn compare-q [query & args]
  (let [datomic-result (apply d/q query args)
        mutomic-result (apply mu/q query args)]
    (= datomic-result mutomic-result)))

(defn diff-q [query & args]
  (let [datomic-result (apply d/q query args)
        mutomic-result (apply mu/q query args)]
    (set/difference datomic-result mutomic-result)))

(defn compare-chapter [queries]
  (doseq [query queries]
    (let [[query args] (if (vector? query)
                         [(first query) (rest query)]
                         [query '()])
          same-result? (apply compare-q query (cons movies args))]
     (if same-result?
       (println "query" (pr-str query) "ok")
       (println (pr-str query) "differs:" (pr-str (apply diff-q query (cons movies args))))))))

;; # Extensible Data Notation

(def ch0-find-all-titles
  '{:find [?t]
    :where [[?m :movie/title ?t]]})

(def ch0
  [ch0-find-all-titles])

;; # Basic Queries

(def ch1-movies-from-1987
  '{:find [?m]
    :where [[?m :movie/year ?y]
            [(= ?y 1987)]]})

(def ch1-movies-with-titles
  '{:find [?m ?t]
    :where [[?m :movie/title ?t]]})

(def ch1-people
  '{:find [?name]
    :where [[?p :person/name ?name]]})

(def ch1
  [ch1-movies-from-1987
   ch1-movies-with-titles
   ch1-people])

;; # Data Patterns

(def ch2-titles-from-1985
  '{:find [?t]
    :where [[?m :movie/title ?t]
            [?m :movie/year 1985]]})

(def ch2-alien-release-year
  '{:find [?y]
    :where [[?m :movie/title "Alien"]
            [?m :movie/year ?y]]})

(def ch2-robocop-director
  '{:find [?name]
    :where [[?m :movie/title "RoboCop"]
            [?m :movie/director ?p]
            [?p :person/name ?name]]})

(def ch2-directors-of-schwarzenegger
  '{:find [?name]
    :where [[?m :movie/cast ?a]
            [?a :person/name "Arnold Schwarzenegger"]
            [?m :movie/director ?d]
            [?d :person/name ?name]]})

(def ch2
  [ch2-titles-from-1985
   ch2-alien-release-year
   ch2-robocop-director
   ch2-directors-of-schwarzenegger])

;; # Parameterized Queries

(def ch3-titles-by-year
  '{:find [?title]
    :in [$ ?year]
    :where [[?m :movie/title ?title]
            [?m :movie/year ?year]]})

(def ch3-year-of-titles
  '{:find [?title ?year]
    :in [$ [?title ...]]
    :where [[?m :movie/title ?title]
            [?m :movie/year ?year]]})

(def ch3-collaborations
  '{:find [?title]
    :in [$ ?actor ?director]
    :where [[?m :movie/title ?title]
            [?m :movie/cast ?a]
            [?a :person/name ?actor]
            [?m :movie/director ?d]
            [?d :person/name ?director]]})

(def ch3-ratings-by-actor
  '{:find [?title ?rating]
    :in [$ ?actor [[?title ?rating]]]
    :where [[?m :movie/title ?title]
            [?m :movie/cast ?a]
            [?a :person/name ?actor]]})

(def ratings
  [["Die Hard" 8.3]
   ["Alien" 8.5]
   ["Lethal Weapon" 7.6]
   ["Commando" 6.5]
   ["Mad Max Beyond Thunderdome" 6.1]
   ["Mad Max 2" 7.6]
   ["Rambo: First Blood Part II" 6.2]
   ["Braveheart" 8.4]
   ["Terminator 2: Judgment Day" 8.6]
   ["Predator 2" 6.1]
   ["First Blood" 7.6]
   ["Aliens" 8.5]
   ["Terminator 3: Rise of the Machines" 6.4]
   ["Rambo III" 5.4]
   ["Mad Max" 7.0]
   ["The Terminator" 8.1]
   ["Lethal Weapon 2" 7.1]
   ["Predator" 7.8]
   ["Lethal Weapon 3" 6.6]
   ["RoboCop" 7.5]])

(def ch3
  [[ch3-titles-by-year 1988]
   [ch3-year-of-titles ["Lethal Weapon" "Lethal Weapon 2" "Lethal Weapon 3"]]
   [ch3-collaborations "Michael Biehn" "James Cameron"]
   [ch3-ratings-by-actor "Mel Gibson" ratings]])

;; # More queries

(def ch4-attributes-of-movie
  '{:find [?attr]
    :in [$ ?title]
    :where [[?m :movie/title ?title]
            [?m ?attr ?v]
            #_[?a :db/ident ?attr]]})

(def ch4-crew-of-movie
  '{:find [?name]
    :in [$ ?title [?attr ...]]
    :where [[?m :movie/title ?title]
            [?m ?attr ?p]
            [?p :person/name ?name]]})

;(def ch4-schema ...)
;(def ch4-data-import-time ...)

(def ch4
  [[ch4-attributes-of-movie "Commando"]
   [ch4-crew-of-movie "Die Hard" [:movie/cast :movie/director]]])

;; # Predicates

(def ch5-movies-older-than
  '{:find [?title]
    :in [$ ?year]
    :where [[?m :movie/title ?title]
            [?m :movie/year ?y]
            [(<= ?y ?year)]]})

(def ch5-actors-older-than
  '{:find [?actor]
    :in [$ ?compare-actor]
    :where [[?c :person/name ?compare-actor]
            [?c :person/born ?cy]
            [?m :movie/cast ?p]
            [?p :person/name ?actor]
            [?p :person/born ?y]
            [(< ?y ?cy)]]})

; fails because ?r is unresolved when it's used in mutomic because relations are resolved last.
(def ch5-good-new-movies
  '{:find [?title]
    :in [$ ?year ?rating [[?title ?r]]]
    :where [[?m :movie/title ?title]
            [?m :movie/year ?y]
            [(>= ?y ?year)]
            [(>  ?r ?rating)]]})

(def ch5
  [[ch5-movies-older-than 1979]
   [ch5-actors-older-than "Danny Glover"]
   [ch5-good-new-movies 1990 8.0 ratings]])

;; # Transformation functions

(defn age [birthday today]
  (quot (- (.getTime today)
           (.getTime birthday))
        (* 1000 60 60 24 365)))

(def ch6-people-by-age
  '{:find [?name]
    :in [$ ?age ?today]
    :where [[?p :person/name ?name]
            [?p :person/born ?birthday]
            [(learn-datalog-today/age ?birthday ?today) ?age]]})

(def ch6-younger-than
  '{:find [?name ?age]
    :in [$ ?cname ?today]
    :where [[?c :person/name ?cname]
            [?c :person/born ?cborn]
            [?p :person/name ?name]
            [?p :person/born ?born]
            [(learn-datalog-today/age ?cborn ?today) ?cage]
            [(learn-datalog-today/age ?born ?today) ?age]
            [(< ?age ?cage)]]})

(defn same-day? [d1 d2]
  (and (= (.getDate d1) (.getDate d2))
       (= (.getMonth d1) (.getMonth d2))))

; noticeable slowdown for mutomic. datomic must be doing some magic. (or simple query optimization?)
(def ch6-birthday-paradox
  '{:find [?n1 ?n2]
    :where [[?p1 :person/name ?n1]
            [?p1 :person/born ?d1]
            [?p2 :person/name ?n2]
            [?p2 :person/born ?d2]
            [(learn-datalog-today/same-day? ?d1 ?d2)]
            [(< ?n1 ?n2)]]})

(defn birthday-benchmark
  "surprisingly we achieve similar speeds when not assembling the results in a set.

so maybe we should try doing that while assembling the results and only optimize
the order of the clauses after that. (it's really quite a bit confusing...)"
  []
  (let [birthday-opt '[[?p1 :person/born ?d1]
                       [?p2 :person/born ?d2]
                       [(learn-datalog-today/same-day? ?d1 ?d2)]
                       [?p1 :person/name ?n1]
                       [?p2 :person/name ?n2]
                       [(< ?n1 ?n2)]]
        vars '[?n1 ?n2]]
    (prn "datomic with better clause ordering")
    (time (d/q {:find vars :where birthday-opt} movies))
    (prn "datomic with original order")
    (time (d/q ch6-birthday-paradox movies))
    (prn "mutomic with better clause ordering")
    (time (mu/resolve-var* {} birthday-opt {'$ movies}))
    (prn "mutomic with original order")
    (time (mu/resolve-var* {} (:where ch6-birthday-paradox) {'$ movies}))
    (prn "mutomic with index")
    (time (mu/resolve-var* {} birthday-opt {'$ movies-db}))
    (prn "mutomic with putting the results in a set")
    (time (into #{}
                (map (fn [env]
                       (mapv env vars))
                     (mu/resolve-var* {} birthday-opt {'$ movies}))))))

(def ch6
  [[ch6-people-by-age 63 #inst "2013-08-02"]
   [ch6-younger-than "Bruce Willis" #inst "2013-08-02"]
   ch6-birthday-paradox])

;; # Aggregates

;(def ch7-number-of-movies)
;(def ch7-methusalem)
;(def ch7-avg-actor-ratings)

;; # Rules

(def ch8-movie-year
  '{:find [?title]
    :in [$ %]
    :where [(movie-year ?title 1991)]})

(def ch8-colleagues
  '{:find [?colleague]
    :in [$ ?name %]
    :where [[?p1 :person/name ?name]
            (colleagues ?p1 ?p2)
            [?p2 :person/name ?colleague]]})

(def ch8-colleagues-rule
  '[[(colleagues ?p1 ?p2)
     [?m :movie/cast ?p1]
     [?m :movie/cast ?p2]
     [(not= ?p1 ?p2)]]
    [(colleagues ?p1 ?p2)
     [?m :movie/cast ?p1]
     [?m :movie/director ?p2]]
    [(colleagues ?p1 ?p2) (colleagues ?p2 ?p1)]])

(def ch8-sequels
  '{:find [?sequel]
    :in [$ % ?title]
    :where [[?m :movie/title ?title]
            (sequels ?m ?s)
            [?s :movie/title ?sequel]]})

(def ch8-sequels-rule
  '[[(sequels ?m1 ?m2)
     [?m1 :movie/sequel ?m2]]
    [(sequels ?m1 ?m2)
     [?m1 :movie/sequel ?m]
     (sequels ?m ?m2)]])

(def ch8
  [[ch8-movie-year '[[(movie-year ?title ?year)
                      [?m :movie/title ?title]
                      [?m :movie/year ?year]]]]
   [ch8-colleagues "Sigourney Weaver" ch8-colleagues-rule]
   [ch8-sequels ch8-sequels-rule "Mad Max"]])

;; # Fin

(defn compare-all []
  (doseq [ch [ch0 ch1 ch2 ch3 ch4 ch5 ch6 ch8]]
    (compare-chapter ch)))

;(compare-all)
