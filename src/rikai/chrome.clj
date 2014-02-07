(ns rikai.chrome
  (:require [rikai.util :as u])
  (:import [java.util Date]))

(defn line->url [[id url title]]
  (let [title (if (and (seq? title) (> (count title) 0))
                (.substring 1 title (dec (count title))))]
    (conj {:id (u/parse-int id)
           :url url}
          (if title {:title title}))))

(defn chrome-date [s]
  (if-let [n (u/parse-long s)]
    (Date. (long (/ (- n 11644473600000000) 1000)))))

(defn visit-type [type]
  (case (bit-and type 0xff)
    (0 2 3 4) :link
    (1 5 8 9 10) :typed))

(defn line->visit [[id from from-valid? url-ref date type]]
  (let [from (u/parse-int from)
        from-valid? (= 1 (u/parse-int from-valid?))]
    (conj {:id (u/parse-int id)
           :url-ref (u/parse-int url-ref)
           :date (chrome-date date)
           :type (visit-type (u/parse-int type))}
          (if from-valid? {:from from}))))

