(ns rikai.firefox
  (:require [clojure.string :as string])
  (:import [java.util Date]))

(defn parse-int [n]
  (try
    (Integer/parseInt n)
    (catch NumberFormatException ne
      nil)))

(defn ->hist-entry [[url title last-visited]]
  (let [title (if (and (seq? title) (> (count title) 0))
                (.substring 1 title (dec (count title))))
        last-visited (parse-int last-visited)
        t (type last-visited)]
    (conj {:url url}
          (if title {:title title})
          (if last-visited {:last-visited (Date. (unchecked-divide-int last-visited 1000))}))))

(def firefox-history
  (->>
    (slurp "ff-history.csv")
    string/split-lines
    (map #(string/split % #","))
    (map ->hist-entry)))

;(identity (filter #(.contains (:url %) "oops") firefox-history))