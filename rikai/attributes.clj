(ns rikai.attributes
  (:use [rikai.datomic-utils :only (attr-def)]))

(def default
  [(attr-def -1 :url :db.type/uri :db.cardinality/one)

   (attr-def -2 :tags :db.type/ref :db.cardinality/many)
   (attr-def -3 :name :db.type/string :db.cardinality/one {:db/unique :db.unique/identity})

   (attr-def -4 :title :db.type/string :db.cardinality/one {:db/index true})
   (attr-def -5 :note :db.type/string :db.cardinality/many)
   (attr-def -6 :time :db.type/instant :db.cardinality/one)

   (attr-def -7 :latitude  :db.type/double :db.cardinality/one)
   (attr-def -8 :longitude :db.type/double :db.cardinality/one)
   (attr-def -9 :address :db.type/string :db.cardinality/one)

   (attr-def -10 :related :db.type/ref :db.cardinality/many)])
