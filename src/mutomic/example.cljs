(ns mutomic.example
  (:require [mutomic :as mu]
            [mutomic.sample-data :as s]
            [mutomic.natural :as n]
            [clojure.string :as str]
            [cljs.reader :as edn]))

(enable-console-print!)

(prn
 (mu/q '{:find [?name]
         :where [[_ :name ?name]]}
       s/fred-julia-joe))

(defn ^:export fancy? [name]
  (> (count name) 4))

(prn
 (mu/q '{:find [?name ?age]
         :where [[?p :name ?name]
                 [?p :age ?age]
                 [(mutomic.example/fancy? ?name)]]}
       s/fred-julia-joe))

(defn answer [question-el answer-el data-el]
  (fn [ev]
    (let [data (.-value data-el)
          data (if (str/blank? data)
                 s/fred-julia-joe
                 (edn/read-string data))
          question (.-value question-el)
          res (n/mq question data)]
      (set! (.-textContent answer-el) (pr-str res)))))

(defn ^:export setup-qa [question-id answer-id data-id]
  (let [question-el (js/document.getElementById question-id)
        answer-el (js/document.getElementById answer-id)
        data-el (js/document.getElementById data-id)
        answer-fn (answer question-el answer-el data-el)]
    (when (str/blank? (.-value data-el))
      (set! (.-value data-el) (pr-str mu/fred-julia-joe)))
    (.addEventListener question-el "input" answer-fn)
    (.addEventListener question-el "keyup"
                       (fn [ev]
                         (if (= (.-keyCode ev) 13)
                           (answer-fn ev))))))
