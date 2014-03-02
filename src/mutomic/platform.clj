(ns mutomic.platform
  "Host platform dependent code."
  (:refer-clojure :exclude [resolve])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn starts-with [string start]
  (.startsWith string start))

(defn to-upper-case [string]
  (.toUpperCase string))

(defn illegal-argument [msg]
  (throw (IllegalArgumentException. msg)))

(defn resolve [sym]
  (clojure.core/resolve sym))

(defn read-edn [readers f]
  (edn/read {:readers readers} (java.io.PushbackReader. (io/reader f))))
