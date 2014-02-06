(ns rikai.pinboard
  (:require [clojure.string :as string]
            [clojure.xml :as xml]

            [rikai.datomic-utils :as du]
            [datomic.api :as d])
  (:import [java.net URI]))

(defn ->post [{:keys [attrs]}]
  (let [{:keys [href description extended time tag shared]} attrs]
    (conj
     {:url (URI. href)
      :title description
      :note extended
      :time (du/string->datomic-value time :db.type/instant)
      :tags (if (string/blank? tag) [] (string/split tag #" "))}
     (if (= shared "yes") {:public true}))))

(defn parse-posts [file]
  (map ->post (:content (xml/parse file))))

(defn ->tags [posts]
  (into #{} (mapcat :tags posts)))

(defn ->tx-data [posts]
  (let [next-id (du/tempid-maker)
        tempid (fn [] (d/tempid :db.part/user (next-id)))
        [tags tag->id] (reduce (fn [[tags tag->id] tag]
                                 (let [id (tempid)]
                                   [(conj tags (assoc {:name tag} :db/id id)) (assoc tag->id tag id)]))
                               [[] {}]
                               (->tags posts))]
    (concat tags
            (map (fn [post]
                   (update-in (assoc post :db/id (tempid))
                              [:tags]
                              #(map tag->id %)))
                 posts))))

(d/transact (d/connect "datomic:free://localhost:4334/rikai")
  (->tx-data (parse-posts "pinboard_2014-02-06.xml")))
