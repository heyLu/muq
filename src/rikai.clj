(ns rikai
  "a place for facts to live"
  (:require [clojure.walk :as walk]
            [datomic.api :as d]
            [rikai.attributes :as attr])
  (:import [java.util Date]
           [java.net URI]))

(def db-url "datomic:free://localhost:4334/rikai")

(def conn
  (do
    (d/create-database db-url)
    (d/connect db-url)))

(defn db []
  (d/db conn))

(attr/init conn)

(d/transact conn
  [{:db/id (d/tempid :db.part/user -1)
    :url (URI. "http://news.papill0n.org")}])

(d/q '[:find ?url
       :where [?e :url ?url]]
     (db))

(def arashi-url
  (first (d/q '[:find ?e ?url
                :where [?e :url ?url]]
              (db))))

(d/transact conn
  [{:db/id (d/tempid :db.part/user -1)
    :tag "project"}
   {:db/id (first arashi-url)
    :tags [(d/tempid :db.part/user -1)]}])

(d/q '[:find ?url ?tag
       :where [?e :url ?url]
              [?e :tags ?t]
              [?t :tag ?tag]]
     (db))

(d/q '[:find ?e ?tag
       :where [?e :tag ?tag]]
     (db))

(defn def-attr [id ident valueType cardinality & [more-attrs]]
  (merge {:db/id (d/tempid :db.part/db id)
          :db/ident ident
          :db/valueType valueType
          :db/cardinality cardinality
          :db.install/_attribute :db.part/db}
         more-attrs))

(defn log [content & [time]]
  {:note content
   :time (or time (Date.))})

; let's make our computer understand a few things. what about telling it what
; tags are?
; we can tag things. things are entities. they have an identity. they are
; unique and we can discern them from other things.
; so to tag a url we look what is identified by that url and add the tag to
; it. an entity can have multiple tags, so we add it to a vector or something.

(log
"weird. started the day expecting to work on a sketch of how we'd record
data about ourselves, instead added offset & search to arashi, did nothing
in particular and watched the remaining coding math videos.

tomorrow? probably finally starting on a real prototype?"
 #inst "2014-02-01")

(log "working on a rikai sketch")

(defn find-by {:db-fn true}
  [db k v]
  (ffirst (d/q [:find '?e
                :where ['?e k v]]
               db)))

(find-by (db) :url (URI. "http://news.papill0n.org"))

(defn create-or-get [db k v & [id]]
  (or (find-by db k v)
      {:db/id (d/tempid :db.part/user (or id -1))
       k v}))

(create-or-get (db) :url (URI. "http://pixl.papill0n.org") -2)

(defn tag {:db-fn true}
  [db eid tag]
  (let [tid (create-or-get db :tag tag)]
    (if (number? tid)
      [{:db/id eid
        :tags tid}]
      [tid
       {:db/id eid
        :tags (:db/id tid)}])))

(tag (db) (find-by (db) :url (URI. "http://news.papill0n.org")) "i made this")

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

(with-db (db)
  (let [e (find-by :url (URI. "http://news.papill0n.org"))]
    (tag e "i made this")))

(with-db (db)
  (tag (find-by :url (URI. "http://news.papill0n.org")) "i made this"))

(walk/macroexpand-all
  '(with-db (db)
     (let [e (find-by :url (URI. "http://news.papill0n.org"))]
       (tag e "i made this"))))

(defn tagged-with {:db-fn true}
  [db tag]
  (d/q '[:find ?e
         :in $ ?tag
         :where [?e :tags ?t]
                [?t :tag ?tag]]
       db tag))

(with-db (db)
  (tagged-with "project"))

(defn visit [address [lat lon] name time & [more-attrs]]
  (merge {:db/id (d/tempid :db.part/user -1)
          :latitude lat
          :longitude lon
          :name name
          :time time}
         more-attrs))

(d/transact conn
  [(visit "merseburger straße 95, leipzig altlindenau"
          [15.5332 12.3307]
          "m. ellebrecht, ?, ?"
          #inst "2014-02-03T19:00:00+01:00"
          {:url (URI. "https://www.wg-gesucht.de/4291948.html")})])

;(defentity fn-name [x y z] body) ;=> (defn fn-name [x y z & [more-attrs]] (merge body more-attrs {:db/id ...}))