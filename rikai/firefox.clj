(ns rikai.firefox
  (:require [clojure.string :as string]
            [datomic.api :as d]
            [rikai :as r])
  (:import [java.util Date]))

(defn parse-int [n]
  (try
    (Integer/parseInt n)
    (catch NumberFormatException ne
      nil)))

(defn parse-long [n]
  (try
    (Long/parseLong n)
    (catch NumberFormatException ne
      nil)))

(defn firefox-date [s]
  (if-let [n (parse-long s)]
    (Date. (long (/ n 1000)))))

(defn ->hist-entry [[id url title last-visited]]
  (let [title (if (and (seq? title) (> (count title) 0))
                (.substring 1 title (dec (count title))))
        last-visited (firefox-date last-visited)]
    (conj {:id (parse-int id)
           :url url}
          (if title {:title title})
          (if last-visited {:last-visited last-visited}))))

(defn read-csv [file]
  (->> (slurp file) string/split-lines (map #(string/split % #","))))

(def firefox-history
  (->>
    (read-csv "ff-history.csv")
    (map ->hist-entry)))

;(identity (filter #(.contains (:url %) "oops") firefox-history))

(defn visit-type [type]
  "http://mxr.mozilla.org/mozilla-central/source/toolkit/components/places/nsINavHistoryService.idl#1173"
  (case type
    (1 4 5 6 8) :link
    2 :typed
    3 :bookmark
    7 :download))

(defn ->visit [[id from-visit from-valid? hist-id date type]]
  (let [from (parse-int from-visit)
        from-valid? (= 1 (parse-int from-valid?))]
    (conj {:id (parse-int id)
           :url-ref (parse-int hist-id)
           :date (firefox-date date)
           :type (visit-type (parse-int type))}
          (if from-valid? {:from from}))))

(def firefox-visits
  (->>
    (read-csv "ff-visits.csv")
    (map ->visit)))

(count (filter :from firefox-visits))

(defn connect-to [db-url]
  (d/create-database db-url)
  (d/connect db-url))

(def conn (connect-to "datomic:mem://history"))
(defn db [] (d/db conn))

(d/transact conn
  [(r/def-attr -1 :url :db.type/string :db.cardinality/one)
   (r/def-attr -2 :title :db.type/string :db.cardinality/one)
   (r/def-attr -3 :date :db.type/instant :db.cardinality/one)
   (r/def-attr -4 :from :db.type/ref :db.cardinality/one)
   (r/def-attr -5 :url-ref :db.type/ref :db.cardinality/one)
   (r/def-attr -6 :type :db.type/keyword :db.cardinality/one)])

(defn hist-entry->datom [{:keys [id] :as entry}]
  (assoc (dissoc entry :id :last-visited)
    :db/id (d/tempid :db.part/user (- id))))

(defn visit->datom [{:keys [id url-ref from] :as visit}]
  (conj (dissoc visit :id)
        {:db/id (d/tempid :db.part/user (- -50000 id))
         :url-ref (d/tempid :db.part/user (- url-ref))}
        (if from {:from (d/tempid :db.part/user (- -50000 from))})))

(d/transact conn
  (concat (map hist-entry->datom firefox-history)
          (map visit->datom firefox-visits)))

(def visit->url
  '[(url-of ?visit ?url)
    [?visit :url-ref ?url-ref]
    [?url-ref :url ?url]])

(defn visit-chain [n]
  (let [names (map #(symbol (str "?v" %)) (range n))]
    (apply conj
      [(cons 'visit-chain (reverse names))]
      (map (fn [[v1 v2]]
             [v1 :from v2])
           (partition 2 1 names)))))

(d/q '[:find ?from_waxy ?date
       :in $ %
       :where [?v1 :from ?v2]
              (url-of ?v2 ?url)
              [(.contains ?url "waxy.org")]
              (url-of ?v1 ?from_waxy)
              [?v1 :date ?date]]
     (db)
     [visit->url])

(d/q '[:find ?url
       :where [_ :url ?url]
              [(.contains ?url "fogus.me")]]
     (db))

(d/q '[:find ?url
       :in $ %
       :where (url-of ?start "http://news.papill0n.org/")
              (visit-chain ?start _ _ _ ?end)
              (url-of ?end ?url)]
     (db)
     [visit->url (visit-chain 5)])