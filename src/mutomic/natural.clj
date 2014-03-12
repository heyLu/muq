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
    (map #(get % '?who) (mu/resolve-var* {} clauses {'$ mu/fred-julia-joe} #{}))))

(comment
  (nq "Wer ist awkward")

  (nq "Wer mag Julia")
  ;=> ["Wer" "mag" "Fred" "?"] -> [["Wer" :subject] ["mag" :predicate] ["Fred" :object]]
)

(defn ^:export mq [question]
  (let [matchers [[#"Who likes (\w+)" (fn [name]
                                        [['?who :likes '?p]
                                         ['?p :name name]])]
                  [#"Who is liked by (\w+)" (fn [name]
                                              [['?p1 :name name]
                                               ['?p1 :likes '?p2]
                                               ['?p2 :name '?who]])]
                  [#"Who is (\w+)" (fn [trait]
                                     [['?who :is trait]])]
                  [#"Who is older than (\d+)" (fn [age-str]
                                                [['?who :age '?age]
                                                 [(list '> '?age (p/parse-int age-str))]])]
                  [#"Who is older than (\w+) years" (fn [name]
                                                      [['?p :name name]
                                                       ['?p :age '?page]
                                                       ['?who :age '?age]
                                                       ['(> ?age ?page)]])]]
        [re f] (first
                (filter (fn [[re _]]
                          (re-find re question))
                        matchers))
        clauses (apply f (subvec (re-find re question) 1))]
    (mu/q {:find ['?who]
           :where clauses}
          mu/fred-julia-joe)))

(comment
  (mq "Who likes Joe?")
  (mq "Who is liked by Joe?")
  (mq "Who is awesome?")
  (mq "Who is older than 3 years?")
  (mq "Who is older than Julia?"))
