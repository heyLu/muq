(ns rikai.scratchpad
  "A place to experiment with things."
  (:require [datomic.api :as d]

            [rikai :as r]))

(def db-url "datomic:mem://rikai")

(def conn
  (do
    (d/create-database db-url)
    (d/connect db-url)))

(defn db []
  (d/db conn))

; play!
