(ns fluree.hnsw.insert
  (:require [fluree.hnsw.item :as item]
            [fluree.hnsw.search :as search]
            [fluree.hnsw.layer :as layer]
            [fluree.hnsw.db :as db]))

(set! *warn-on-reflection* true)

(defn shrink-edges
  [item layer max-k]
  (let [neighbor-ids (item/edges item)]
    (if (> (count neighbor-ids) max-k)
      (let [neighbors  (map #(layer/item layer %) neighbor-ids)
            neighbors* (search/closest-neighbors item neighbors max-k)]
        (item/replace-edges item (map item/id neighbors*)))
      item)))

(defn add-edges
  [layer max-k new-item neighbors]
  ;; first add neighbors as edges to new item, and add to layer
  (let [new-item* (item/replace-edges new-item (into #{} (map item/id neighbors)))
        new-id    (item/id new-item*)
        layer*    (assoc layer new-id new-item*)]
    ;; iterate over each neighbor and add the new item as an edge
    (reduce
     (fn [layer neighbor]
       (let [neighbor* (-> (item/add-edge neighbor new-id)
                           (shrink-edges layer max-k))]
         (assoc layer (item/id neighbor) neighbor*)))
     layer*
     neighbors)))

(defn first-insert
  "First insert puts the new item at every layer,
  there are not yet any edges/neighbors to consider."
  [db new-item]
  (let [layers (mapv
                #(assoc % (item/id new-item) new-item)
                (db/layers db))]
    (db/new-db db layers new-item)))

(defn stage
  [db {:keys [id values] :as v-map}]
  (let [db       (if (db/staged? db)
                   db
                   (db/stage! db))
        t        (db/t db)
        metric   (db/metric db)
        new-item (item/create v-map metric t)]
    (if (db/empty? db)
      (first-insert db new-item)
      (let [max-k          (db/max-k db)
            efConstruction (db/efConstruction db)
            starting-layer (db/assign-layer db nil)
            layers         (db/layers db)
            starting-point (search/entrance-point db new-item starting-layer) ;; closest entry point on target layer-i
            ]
        (loop [insertion-layers (range starting-layer -1 -1) ;; insert into every layer below starting layer
               entry-point      starting-point
               layers*          layers]
          (if-let [next-layer-id (first insertion-layers)]
            (let [layer        (get layers* next-layer-id) #_(db/get-layer db next-layer-id)
                  entry-point* (layer/item layer (item/id entry-point)) ;; update entry-point for the current layer (might have different edges than prior layer entry point)
                  candidates   (search/search-layer layer new-item entry-point* efConstruction)
                  max-k*       (if (= next-layer-id 0) ;; for base layer, double insertion points
                                 (* 2 max-k)
                                 max-k)
                  neighbors    (take max-k* candidates) ;; TODO - how different should max-k be than efConstruction? - is there a reasonable default we can use?
                  layer*       (add-edges layer max-k* new-item neighbors)]
              (recur (rest insertion-layers)
                     (first neighbors)
                     (assoc layers* next-layer-id layer*)))
            (db/new-db db layers*)))))))
