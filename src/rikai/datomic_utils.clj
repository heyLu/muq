(ns rikai.datomic-utils
  (:require [clojure.walk :as walk]
            [datomic.api :as d]
            [rikai.util :as u]
            [clojure.edn :as edn])
  (:import [java.util Date UUID]
           [java.net URI]
           [datomic.db DbId]))

(defn find-by {:db-fn true}
  [db k v]
  (ffirst (d/q [:find '?e
                :where ['?e k v]]
               db)))

(defn db-fn? [f]
  (:db-fn (meta f)))

(defmacro with-db* [db-sym form]
  (walk/prewalk (fn [form]
                  (if (and (list? form) (db-fn? (resolve (first form))))
                    (conj (rest form) db-sym (first form))
                    form))
                 form))

(defmacro with-db [db form]
  `(let [db# ~db]
     (with-db* db# ~form)))

(defn string->datomic-value [s type]
  (case type
    :db.type/keyword (keyword s)
    :db.type/string s
    :db.type/boolean (= s "true")
    :db.type/long (u/parse-long s)
    ;:db.type/bigint
    ;:db.type/float
    ;:db.type/double
    ;:db.type/bigdec
    :db.type/ref (u/parse-long s)
    :db.type/instant (edn/read-string (str "#inst " \" s \"))
    ;:db.type/uuid
    :db.type/uri (URI. s)
    ;:db.type/bytes
    ))

(defn find-by-str {:db-fn true}
  [db k v]
  (let [attr-type (:db/valueType (d/entity db k))
        v (string->datomic-value v attr-type)]
    (find-by db k v)))

(defn guess-type [v]
  (cond
    (keyword? v) :db.type/keyword
    (string? v) :db.type/string
    (instance? Boolean v) :db.type/boolean
    (instance? Long v) :db.type/long
    (instance? BigInteger v) :db.type/bigint
    (instance? Float v) :db.type/float
    (instance? Double v) :db.type/double
    (instance? BigDecimal v) :db.type/bigdec
    (instance? DbId v) :db.type/ref
    (instance? Date v) :db.type/instant
    (instance? UUID v) :db.type/uuid
    (instance? URI v) :db.type/uri
    (instance? (type (byte-array [])) v) :db.type/bytes
    (map? v) (into {}
                   (map (fn [[k v]]
                          [k (guess-type v)])
                        v))
    (vector? v) (mapv guess-type v)
    (coll? v) (map guess-type v)))

(defn guess-attr-defs [v]
  (let [type (guess-type v)]
    (cond
      (map? v) ())))

{:title "heya!"
 :tags ["greeting" "example"]}
; not typable, maybe with a default?
{:title "heya!"
 :tags [{:tag "greeting"} {:tag "example"}]}
; restricted, most data doesn't look like this

(defn attr-def [id ident valueType cardinality & [more-attrs]]
  (merge {:db/id (d/tempid :db.part/db id)
          :db/ident ident
          :db/valueType valueType
          :db/cardinality cardinality
          :db.install/_attribute :db.part/db}
         more-attrs))

(defn tempid-maker [& [start]]
  (let [next-id (atom (or start -1))]
    (fn []
      (inc (swap! next-id dec)))))
