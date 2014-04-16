(ns muq.sample-data
  (:require [muq :as mu]))

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

(def fred-julia-joe-index
  (mu/index-many nil fred-julia-joe))
