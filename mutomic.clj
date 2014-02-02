(ns mutomic
  "A µcroscopic in-memory temporal database in memory.

Trying to understand datomic, mostly.")

(def next-entity-id (atom 0))

(defn next-id []
  (dec (swap! next-entity-id inc)))

(defn id-maker []
  (let [eid (atom -1)]
    (fn []
      (inc (swap! eid dec)))))

(defn assign-real-ids [tx-data]
  (reduce (fn [[res ids] [tid & rs]]
            (let [[ids eid] (if-let [id (ids tid)]
                              [ids id]
                              (let [id (next-id)]
                                [(assoc ids tid id) id]))]
              [(conj res (apply conj [eid] rs)) ids]))
          [[] {}]
          tx-data))

(defn tx->datoms [{e :id :as tx}]
  (map (fn [[a v]]
         [e a v])
       (dissoc tx :id)))

(defn tx-data->datoms [tx-data]
  (reduce (partial apply conj)
          []
          (map tx->datoms tx-data)))

(defn transact [db tx-data]
  ; decompose into datoms
  ; assign ids (replace temp ids)
  ; resolve refs (replace all :db.type/ref attribute values with assigned ids)
  ; check attribute types
  (->>
    (tx-data->datoms tx-data)
    assign-real-ids))

(defn index [idx idx-type datom]
  (let [[e a v t] datom
        k (condp = idx-type
            :eavt [e a v t]
            :aevt [a e v t]
            ; :db/index = true
            :avet [a v e t]
            ; :db.type/ref, reverse index
            :vaet [v a e t])]
    (update-in idx k conj e)))

(def entities
  [{:id -1
    :url "http://github.com/heyLu"
    :title "heyLu (Lucas Stadler)"}
   {:id -2
    :type :visit
    :to -1
    :time #inst "2014-02-01T17:57:00+01:00"}])

(defn query [{vars :find, clauses :where} db]
  )

(query [:find ?e :where [?e :age 10]] (make-db {1 {:name "fred", :age 10}
                                                2 {:name "paula", :age 13}
                                                3 {:name "?", :age -1}
                                                4 {:name "paul", :age 10}}))