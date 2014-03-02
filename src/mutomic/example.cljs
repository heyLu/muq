(ns mutomic.example
  (:require [mutomic :as mu]))

(enable-console-print!)

(prn
 (mu/q '{:find [?name]
         :where [[_ :name ?name]]}
       mu/fred-julia-joe))

(defn ^:export fancy? [name]
  (> (count name) 4))

(prn
 (mu/q '{:find [?name ?age]
         :where [[?p :name ?name]
                 [?p :age ?age]
                 [(mutomic.example/fancy? ?name)]]}
       mu/fred-julia-joe))
