(ns rikai.service
  (:require [clojure.edn :as edn]

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

(defn link-to [entity]
  (let [name (or (:title entity) (:name entity) (:db/id entity))]
    [:a {:href (str "/entity/" (:db/id entity) ".html")} name]))

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

(defn entity-with-refs->html [db entity]
  [:div.entity
   (entity->html db entity)
   [:div
    [:h3 "references"]
    (map (fn [[k v]]
           (if-let [ref-attrs (:referenced-by (d/entity db k))]
             (let [v (if (instance? EntityMap v)
                       (:db/id v)
                       v)
                   ref-vals (d/q [:find '?e
                                  :where ['?e (first ref-attrs) '?r]
                                         ['?r k v]] db)]
               (if (seq ref-vals)
                 [:div
                  [:h4 [:a {:href (str "/entity-with-ref/" (first ref-attrs) "/" k "?value=" v)} v]]
                  [:ul
                   (query->html-links db ref-vals)]]))))
         entity)]])

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

; rendering:
;   type: edn, html, (json?)
;   standard or pretty printed (possibly a type because only applies to edn/json)
;   extended info
;  => (render-edn pretty? extended?)
;  => (render-html extended?)
;  but we also have different data types: entity/datum, query result (list of entities)
;  and we also want offset/limit

(defn edn-response [data]
  {:status 200
   :body (pr-str data)
   :headers {"Content-Type" "text/plain"}})

(defn html-response [data]
  {:status 200
   :body (html data)
   :headers {"Content-Type" "text/html"}})

(defn query->renderable [db query-result]
  (reify
    Renderable
    (render [_ req]
      (let [content-type (get-in req [:headers "content-type"])
            entities (map (fn [[eid]] (d/entity db eid)) query-result)]
        (condp = content-type
          "application/edn" (edn-response (map #(into {} %) entities))
          (html-response (query->html db entities)))))))

(defroutes routes
  (GET "/" []
    (-> (file-response "docs.md")
        (header "Content-Type" "text/plain")))
  (GET ["/entity/:id", :id #"[0-9]+"] [id]
    (if-let [entity (d/entity (d/db conn) (u/parse-long id))]
      (http-response 200 (pr-str (into {} entity)) :headers {"Content-Type" "text/plain"})
      (http-response 404 "No such entity")))
  (GET ["/entity/:id.html", :id #"[0-9]+"] [id]
    (let [db (d/db conn)]
      (if-let [entity (d/entity db (u/parse-long id))]
        (http-response 200 (html (entity-with-refs->html db entity)))
        (http-response 404 "No such entity."))))
  (GET "/entity-by/:key" {{:keys [key value]} :params}
    (let [db (d/db conn)]
      (if value
        (if-let [entity (d/entity db (find-by-str db (keyword key) value))]
          (http-response 200 (html (entity-with-refs->html db entity))
                         :headers {"Content-Type" "text/html"})
          (http-response 404 "No such entity."))
        (http-response 404 "Missing parameter: value."))))
  (GET "/entity-with/:key" [key]
    (let [db (d/db conn)]
      (query->renderable db (d/q [:find '?e :where ['?e key '_]] db))))
  (GET "/entity-with-ref/:ref/:key" {{:keys [ref key value]} :params}
    (let [db (d/db conn)
          attr-type (:db/valueType (d/entity db key))
          value (du/string->datomic-value value attr-type)]
      (query->renderable db (d/q [:find '?e
                                  :where ['?e ref '?r]
                                         ['?r key value]] db))))
  (GET "/entity-matching/:key" {{:keys [key value]} :params}
    )
  (POST "/entity" {body :body}
    (d/transact conn
       [(assoc (edn/read body) :db/id (d/tempid :db.part/user -1))])
    "ok")
  (PUT "/entity/:id" {{:keys [id]} :params, body :body}
    (let [eid (d/entity (d/db conn) (u/parse-long id))]
      (d/transact conn
        [(assoc (edn/read body) :db/id eid)]))))

(def app
  (-> routes
      wrap-keyword-params
      wrap-params))
