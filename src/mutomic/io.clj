(ns mutomic.io
  (:require [clojure.data.fressian :as fress]
            [clojure.java.io :as io]))

(defn save! [f idx]
  (with-open [w (fress/create-writer (io/output-stream f))]
    (fress/write-object w idx)))

;(save! (java.io.File. "fjj.idx.fsn") fred-julia-joe-index)

(defn load! [f]
  (with-open [r (fress/create-reader (io/input-stream f))]
    (fress/read-object r)))

;(load! "fjj.idx.fsn")
