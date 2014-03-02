(ns mutomic.example
  (:require [mutomic :as mu]))

(enable-console-print!)

(prn
 (mu/q '{:find [?name]
         :where [[_ :name ?name]]}
       mu/fred-julia-joe))
