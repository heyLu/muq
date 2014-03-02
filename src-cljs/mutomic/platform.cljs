(ns mutomic.platform
  "Host platform dependent code."
  (:require [cljs.reader :as edn]))

(defn starts-with [string start]
  (.startsWith string start))

(defn to-upper-case [string]
  (.toUpperCase string))

(defn illegal-argument [msg]
  (throw (js/Error. (str "Illegal argument: " msg))))

(defn resolve [sym]
  nil)

(defn read-edn [readers f]
  (throw (js/Error. "Not implemented!")))
