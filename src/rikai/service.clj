(ns rikai.service
  (:require [clojure.edn :as edn]
            [clojure.string :as string]

            [rikai.util :as u]
            [rikai.datomic-utils :as du]
            [datomic.api :as d])
  (:use [ring.util.response :only (header file-response)]
        [ring.middleware.params :only (wrap-params)]
        [ring.middleware.keyword-params :only (wrap-keyword-params)]
        [compojure.core :only (defroutes GET POST PUT)]
        [compojure.response :only (Renderable)]
        [hiccup.core :only (html)]
        [rikai.datomic-utils :only (find-by-str with-db)])
  (:import [datomic.query EntityMap]))

(defonce conn
  (d/connect "datomic:free://localhost:4334/rikai"))

(defn http-response [status body & {:as attrs}]
  (into {:status status
         :body body}
        attrs))

(defn edn-response [data]
  {:status 200
   :body (pr-str data)
   :headers {"Content-Type" "text/plain"}})

(defn html-response [data]
  {:status 200
   :body (html data)
   :headers {"Content-Type" "text/html"}})

(defn link-to [entity]
  (let [name (or (:title entity) (:name entity) (:db/id entity))]
    [:a {:href (str "/entity/" (:db/id entity))} name]))

(defn attr->html [type value]
  (case type
    :db.type/ref (link-to value)
    :db.type/uri [:a {:href (str value)} value]
    value))

(defn entity->html [db entity]
  (map (fn [[k v]]
         (let [{type :db/valueType} (d/entity db k)]
           [:div
            [:span.attr (pr-str k)]
            " "
            (if (and (coll? v) (not (instance? EntityMap v)))
              (interpose " " (map #(attr->html type %) v))
              [:span.value (attr->html type v)])]))
       entity))

(defn query->html-links [db query-result]
  (map (fn [[eid]]
         [:li (link-to (d/entity db eid))])
       query-result))

(defn references-of [db entity]
  (mapcat (fn [[k v]]
            (if-let [ref-attrs (:referenced-by (d/entity db k))]
              (let [v (if (instance? EntityMap v)
                        (:db/id v)
                        v)
                    ref-vals (d/q [:find '?e
                                   :where ['?e (first ref-attrs) '?r]
                                   ['?r k v]] db)]
                ref-vals)))
          entity))

(defn entity-with-refs->html [db [entity references]]
  [:div.entity
   (entity->html db entity)
   (if-let [references (seq references)]
     [:div
      [:h3 "references"]
      [:ul
       (query->html-links db references)]])])

(defn entity-matches [entity m]
  (some (fn [[_ v]]
          (.contains (str v) m))
        entity))

(defn filter-results [start limit search results]
  (let [start (or (u/parse-int start) 0)
        limit (or (u/parse-int limit) 100)
        search (or search "")]
    (->> results
         (filter #(entity-matches % search))
         (drop start)
         (take limit))))

(defn ->renderable [html-fn data & args]
  (reify
    Renderable
    (render [_ req]
      (let [content-type (get-in req [:headers "content-type"])
            data (if (and (coll? data) (not (map? data)))
                   (let [{:keys [start count q]} (:params req)]
                     (filter-results start count q data)))]
        (condp = content-type
          "application/edn" (edn-response data)
          (html-response (apply html-fn data args)))))))

(defn entity-with-refs->renderable [db entity]
  (->renderable #(entity-with-refs->html db %) [entity (references-of db entity)]))

; rendering:
;   type: edn, html, (json?)
;   standard or pretty printed (possibly a type because only applies to edn/json)
;   extended info
;  => (render-edn pretty? extended?)
;  => (render-html extended?)
;  but we also have different data types: entity/datum, query result (list of entities)
;  and we also want offset/limit

(defn query->html [db query-result]
  (html [:html
         [:head [:style ".entity { margin-left: 1em; }"]]
         [:body
          [:div
           (map (fn [entity]
                  [:div
                   (link-to entity)
                   [:div.entity
                    (entity->html db entity)]])
                query-result)]]]))

(defn query->renderable [db query-result]
  (->renderable #(query->html db %) (map (fn [[eid]] (d/entity db eid)) query-result)))

(defn split-first [s str-or-char]
  (let [split (str str-or-char)
        idx (.indexOf s split)]
    (if (< idx 0)
      [s]
      [(.substring s 0 idx) (.substring s (+ idx (count split)))])))

(defn ->datomic-val [db k v]
  (let [attr-type (:db/valueType (d/entity db k))]
    (if v
      (du/string->datomic-value v attr-type))))

(defn ->clause [db var s]
  (if (.contains s "->")
    (let [[ref kv] (split-first s "->")
          ref-e (gensym "?e")]
      (apply conj [[var (keyword ref) ref-e]]
            (->clause db ref-e kv)))
    (let [[k v] (split-first s ":")
          kw (keyword k)]
      [[var kw (or (->datomic-val db k v) (gensym "?v"))]])))

(defn ->clauses [db s]
  (apply concat (map #(->clause db '?e %)
                     (string/split s #","))))

(defn read-uri [uri]
  (java.net.URI. uri))

(defn read-edn-string [s]
  (edn/read-string {:readers {'uri read-uri}} s))

(defroutes routes
  (GET "/" []
    (-> (file-response "docs.md")
        (header "Content-Type" "text/plain")))
  (GET ["/entity/:id", :id #"[0-9]+"] [id]
    (let [db (d/db conn)]
      (if-let [entity (d/entity db (u/parse-long id))]
        (entity-with-refs->renderable db (d/touch entity))
        (http-response 404 "No such entity."))))
  (GET "/entity-by/:key" {{:keys [key value]} :params}
    (let [db (d/db conn)]
      (if value
        (if-let [entity (d/entity db (find-by-str db (keyword key) value))]
          (entity-with-refs->renderable db (d/touch entity))
          (http-response 404 "No such entity."))
        (http-response 404 "Missing parameter: value."))))
  (GET "/entities" {{:keys [with]} :params}
    (let [db (d/db conn)
          clauses (->clauses db with)]
      (query->renderable db (d/q (apply conj [:find '?e :where] clauses) db))))
  (GET "/entity" []
    (html [:html
           [:head [:title "New entity"]]
           [:body
            [:form {:id "entity", :action "/entity", :method "post"}
             [:textarea {:form "entity", :name "entity-tx", :rows 20, :cols 80}]
             [:input {:type "submit"}]]]]))
  (POST "/entity" {{:keys [entity-tx]} :params}
        (let [tid (d/tempid :db.part/user -1)
              res (d/transact conn
                              [(assoc (read-edn-string entity-tx) :db/id tid)])
              {:keys [db-after tempids]} @res
              eid (d/resolve-tempid db-after tempids tid)]
          (http-response 303 (str eid)
                         :headers {"Content-Type" "text/plain"
                                   "Location" (str "/entity/" eid)})))
  (PUT "/entity/:id" {{:keys [id]} :params, body :body}
    (let [eid (d/entity (d/db conn) (u/parse-long id))]
      (d/transact conn
        [(assoc (edn/read body) :db/id eid)]))))

(def app
  (-> routes
      wrap-keyword-params
      wrap-params))
