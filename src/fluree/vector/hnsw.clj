(ns fluree.vector.hnsw
  (:require [fluree.vector.hnsw.search :as search]
            [fluree.vector.hnsw.insert :as insert]
            [fluree.vector.hnsw.index :as index]
            [fluree.vector.hnsw.db :as db]
            [mikera.vectorz.core :as v]))

(set! *warn-on-reflection* true)

(defn create
  "Creates a new HNSW index"
  [opts]
  (index/create opts))

(defn db
  "Retrieves the latest db from the index."
  [index]
  (index/db index))

(defn stage
  "Stages a new vector into the HNSW index."
  [db v-map]
  (insert/stage db v-map))

(defn coerce-vectors
  "Insertions allowed as a map, a vector
  or a single item. Coerces them all to a
  sequence of two-tuples of [id vector]."
  [vectors]
  (cond
    (map? vectors)
    [vectors]

    (sequential? vectors)
    vectors

    :else
    (throw
     (ex-info "Invalid insert items. a vector of maps: [{:id ..., :values ...} ...]"
              {:items vectors}))))

(defn upsert
  "Upserts multiple new vector items into provided index.
  Format is:
  [{:id 'vec1', :values [0.5, 1.2, 1.4]},
   {:id 'vec2', :values [0.2, 0.52, -1.3]}
   ...]"
  [index vectors]
  (let [db     (db index)
        items* (coerce-vectors vectors)
        new-db (reduce
                (fn [db* v-map]
                  (stage db* v-map))
                db
                items*)]
    ;; TODO - update state with new db
    new-db))

(defn search
  [db {:keys [vector include-values top-k]}]
  (let [results (search/search db vector (or top-k 5))]
    (if include-values
      (map #(-> (select-keys % [:id :score :t])
                (assoc :values (seq (:vec %)))) results)
      (map #(select-keys % [:id :score :t]) results))))