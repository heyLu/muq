(ns muq.example
  (:require [muq :as mu]
            [muq.sample-data :as s]
            [muq.natural :as n]
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
                 [(muq.example/fancy? ?name)]]}
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
      (set! (.-value data-el) (pr-str s/fred-julia-joe)))
    (.addEventListener question-el "input" answer-fn)
    (.addEventListener question-el "keyup"
                       (fn [ev]
                         (if (= (.-keyCode ev) 13)
                           (answer-fn ev))))))

(defprotocol KeyValueStore
  (kv-read [s k])
  (kv-write [s k v]))

(def localStorage
  (reify
    KeyValueStore
    (kv-read [_ k]
             (when (.key js/localStorage (str k))
               (edn/read-string (aget js/localStorage (str k)))))
    (kv-write [_ k v]
              (aset js/localStorage (str k) (pr-str v)))))

(def query-data
  (atom {:query '{:find [?person]
                  :where [[?person :likes :julia]]}
         :data s/fred-julia-joe}))

(defn load-data! []
  (reset! query-data (kv-read localStorage "muq.example.query")))

(defn run-query! [query answer-el]
  (try
    (let [{:keys [query data]} query
          result (mu/q query data)]
      (set! (.-textContent answer-el) (pr-str result)))
    (catch js/Error e
      (set! (.-textContent answer-el) e))))

(defn ^:export setup-query [query-id answer-id data-id]
  (let [query-el (js/document.getElementById query-id)
        answer-el (js/document.getElementById answer-id)
        data-el (js/document.getElementById data-id)]
    (add-watch query-data :rerun-query
               (fn [_ _ _ new-query]
                 (run-query! new-query answer-el)))
    (add-watch query-data :store-data
               (fn [_ _ old new]
                 (when (not= old new)
                   (kv-write localStorage "muq.example.query" new))))
    (let [init-field! (fn [k el]
                        (set! (.-textContent el) (pr-str (k @query-data)))
                        (.addEventListener el "keyup"
                                           (fn [ev]
                                             (if (and (.-ctrlKey ev) (= (.-keyCode ev) 13))
                                               (try
                                                 (swap! query-data assoc k (edn/read-string (.-value el)))
                                                 (catch js/Error e
                                                   (set! (.-textContent answer-el) (str "Invalid " k ": " e))))))))]
      (init-field! :query query-el)
      (init-field! :data data-el))
    (load-data!)))
