(ns rikai.util
  (:require [clojure.string :as string]))

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

(defn read-csv [file]
  (->> (slurp file) string/split-lines (map #(string/split % #","))))