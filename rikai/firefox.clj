(ns rikai.firefox
  (:require [clojure.string :as string])
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

(defn ->visit [[id from_visit hist_id date type]]
  (let [from (parse-int from_visit)]
    (conj {:id (parse-int id)
           :hist_id (parse-int hist_id)
           :date (firefox-date date)
           :type (visit-type (parse-int type))}
          (if (not= 0 from) {:from from}))))

(def firefox-visits
  (->>
    (read-csv "ff-visits.csv")
    (map ->visit)))

(count firefox-visits)