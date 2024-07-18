(ns fluree.vector.hnsw.item
  (:require [mikera.vectorz.core :as v])
  (:refer-clojure :exclude [vector]))

(set! *warn-on-reflection* true)

(defprotocol VecSimilarity
  (dist [this other] "Distance measurement between the two vectors")
  (reverse-sort? [this] "Returns true if the score is sorted in reverse order"))

(defprotocol ItemProperties
  (id [this] "Unique identifier for the item")
  (vector [this] "Vector the item holds")
  (t [this] "Incremental time (t) of the item")
  (edges [this] "Edges to other items"))

(defrecord DotProductItem
  [id vec add? t edges]
  VecSimilarity
  (dist [_ other]
    (v/dot vec (vector other)))
  (reverse-sort? [_] true)
  ItemProperties
  (id [_] id)
  (vector [_] vec)
  (t [_] t)
  (edges [_] edges))

(defrecord CosineSimilarityItem
  [id vec add? t edges]
  VecSimilarity
  (dist [_ other]
    (/ (v/dot vec (vector other))
       (* (v/magnitude vec) (v/magnitude (vector other)))))
  (reverse-sort? [_] true)
  ItemProperties
  (id [_] id)
  (vector [_] vec)
  (t [_] t)
  (edges [_] edges))

(defrecord EuclideanDistanceItem
  [id vec add? t edges]
  VecSimilarity
  (dist [_ other]
    (v/distance vec (vector other)))
  (reverse-sort? [_] false)
  ItemProperties
  (id [_] id)
  (vector [_] vec)
  (t [_] t)
  (edges [_] edges))

(defn replace-edges
  [item edges]
  (assoc item :edges edges))

(defn add-edge
  [item edge]
  (replace-edges item (conj (edges item) edge)))

(defn create
  ([v-map metric t]
   (create v-map metric t #{}))
  ([{:keys [id values] :as _v-map} metric t edges]
   (case metric
     :distance (->EuclideanDistanceItem id (v/vec values) true t edges)
     :cosine (->CosineSimilarityItem id (v/vec values) true t edges)
     :dotproduct (->DotProductItem id (v/vec values) true t edges))))

