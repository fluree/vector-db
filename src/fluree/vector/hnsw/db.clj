(ns fluree.vector.hnsw.db
  (:require [fluree.vector.hnsw.layer :as layer])
  (:refer-clojure :exclude [empty?]))

(set! *warn-on-reflection* true)

(defn layers
  [db]
  (:layers db))

(defn get-layer
  [db layer-i]
  (get (layers db) layer-i))

(defn entry-point
  [db]
  (:eP db))

(defn top-layer
  [db]
  (:top-layer db))

(defn max-k
  [db]
  (:max-k db))

(defn t
  [db]
  (:t db))

(defn metric
  [db]
  (:metric db))

(defn efConstruction
  [db]
  (:efConstruction db))

(defn staged?
  [db]
  (:staged? db))

(defn level-factor
  [db]
  (:level-factor db))

(defn set-layers
  [db layers]
  (assoc db :layers layers))

(defn set-entry-point
  [db entry-point]
  (assoc db :eP entry-point))

(defn increment-t
  [db]
  (update db :t inc))

(defn next-t
  "Returns what will be the next t value of the db.

  Does not increment t or modify the current db."
  [db]
  (inc (t db)))

(defn assign-layer
  "Returns a layer level that the new item should be placed
   in as a starting point."
  [db item-id]
  (layer/assign-rand (level-factor db) (top-layer db)))

(defn stage!
  "Takes an immutable db and stages it for modification."
  [db]
  (if (staged? db)
    (throw (ex-info (str "DB already staged: " db) {}))
    (-> db
        (increment-t)
        (assoc :staged? true))))

(defn new-db
  ([db layers]
   (-> db
       (set-layers layers)
       (increment-t)))
  ([db layers entry-point]
   (-> db
       (set-layers layers)
       (set-entry-point entry-point)
       (increment-t))))

(defn empty?
  "Returns truthy if the db is empty.

  This is indicated by special :empty keyword"
  [db]
  (nil? (entry-point db)))