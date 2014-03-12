(ns mutomic.natural
  (:require [clojure.string :as str]
            [mutomic :as mu]
            [mutomic.platform :as p]))

(defn flatten-query [nested-query]
  (reduce (fn [queries query]
            (let [cur-query (or (first queries) [])]
              (if (vector? query)
                (let [var (gensym "?v")]
                  (conj (rest queries) (conj cur-query var) (mu/replace-vars {'?q var} query)))
                (conj (rest queries) (conj cur-query query)))))
          '()
          nested-query))

(defn nq [question]
  (let [matches {"Wer" (symbol "?who")
                 "ist" :is
                 "mag" :likes}
        words (str/split question #"\s")
        nested-query (mapv (fn [word]
                             (cond
                              (matches word) (matches word)
                              (and (string? word) (p/upper-case? (first word))) ['?q :name word]
                              :else word))
                           words)
        clauses (flatten-query nested-query)]
    clauses
    (map #(get % '?who) (mu/resolve-var* {} clauses {'$ mu/fred-julia-joe} (atom #{})))))

(comment
  (nq "Wer ist awkward")

  (nq "Wer mag Julia")
  ;=> ["Wer" "mag" "Fred" "?"] -> [["Wer" :subject] ["mag" :predicate] ["Fred" :object]]
)

(defn ^:export mq
  ([question] (mq question mu/fred-julia-joe))
  ([question db]
   (let [matchers [[#"Who likes (\w+)" (fn [name]
                                         [['?who :likes '?p]
                                          ['?p :name name]])]
                   [#"Who is liked by (\w+)" (fn [name]
                                               [['?p1 :name name]
                                                ['?p1 :likes '?p2]
                                                ['?p2 :name '?who]])]
                   [#"Who knows someone who knows (\w+)" (fn [name]
                                                           [['?p :name name]
                                                            '(knows-someone ?who ?p)])]
                   [#"Who knows (\w+)" (fn [name]
                                         [['?p :name name]
                                          '(knows ?who ?p)])]
                   [#"Who is older than (\d+) years" (fn [age-str]
                                                       [['?who :age '?age]
                                                        [(list '> '?age (p/parse-int age-str))]])]
                   [#"Who is older than (\w+)" (fn [name]
                                                 [['?p :name name]
                                                  ['?p :age '?page]
                                                  ['?who :age '?age]
                                                  ['(> ?age ?page)]])]
                   [#"Who is (\w+)" (fn [trait]
                                      [['?who :is trait]])]]
         rules '[[(knows ?p1 ?p2)
                  [?p1 :knows ?p2]]
                 [(knows ?p1 ?p2)
                  [?p1 :likes ?p2]
                  [?p2 :likes ?p1]]
                 [(knows-someone ?p1 ?p2)
                  (knows ?p1 ?p2)
                  [(not= ?p1 ?p2)]]
                 [(knows-someone ?p1 ?p2)
                  (knows ?p1 ?p)
                  (knows-someone ?p ?p2)
                  [(not= ?p1 ?p2)]]]
         [re f] (first
                 (filter (fn [[re _]]
                           (re-find re question))
                         matchers))]
     (if re
       (let [clauses (apply f (subvec (re-find re question) 1))]
         (mu/q {:find ['?who]
                :in '[$ %]
                :where clauses}
               db
               rules))))))

(comment
  (mq "Who likes Joe?")
  (mq "Who is liked by Joe?")
  (mq "Who is awesome?")
  (mq "Who is older than 3 years?")
  (mq "Who is older than Julia?"))
