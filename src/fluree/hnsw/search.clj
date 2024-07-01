(ns fluree.hnsw.search
  (:require [fluree.hnsw.db :as db]
            [fluree.hnsw.item :as item]
            [clojure.data.avl :as avl]))

(set! *warn-on-reflection* true)

(defn add-score
  "Adds a score key to item with score metric to reference."
  [item reference]
  (let [d (item/dist item reference)]
    (assoc item :score d)))

(defn set-score-comparator
  [a b]
  (let [dist-cmp (compare (:score a) (:score b))]
    (if (zero? dist-cmp)
      (compare (item/id a) (item/id b)) ;; if this is also zero, then items are identical
      dist-cmp)))

(defn set-score-comparator-reverse
  [a b]
  (let [dist-cmp (compare (:score b) (:score a))]
    (if (zero? dist-cmp)
      (compare (item/id a) (item/id b)) ;; if this is also zero, then items are identical
      dist-cmp)))

(defn score-sorted-set
  [& ks]
  (let [comparator (if (item/reverse-sort? (first ks))
                     set-score-comparator-reverse
                     set-score-comparator)]
    (apply avl/sorted-set-by (cons comparator ks))))

;; Note: this is the simplest possible version of this routine, and leaves
;    out the distance scaling heuristic.
(defn closest-neighbors
  "Returns closest neighbors to reference as a list,
   sorted by score with maximum length of n."
  [reference candidates n]
  (let [comparator  (if (item/reverse-sort? reference)
                      set-score-comparator-reverse
                      set-score-comparator)
        candidates* (->> candidates
                         (map #(add-score % reference))
                         (sort comparator))]
    ;; if the 'reference' item is in the candidate, remove it
    (if (= (item/id reference) (item/id (first candidates*)))
      (take n (rest candidates*))
      (take n candidates))))

(defn nearest-s
  "Because candidates is an ordered set, nearest will be 'first'"
  [item-set]
  (first item-set))

(defn farthest-s
  "Farthest item in the set."
  [item-set]
  (last item-set))

(defn farther?
  "Returns true if the first item has a lower score than the second item.

  Note scores are sorted depending on the score metric,
  e.g. cosine similarity is opposite sort order than euclidean distance."
  [item-1 item-2]
  (let [cmp (compare (:score item-1) (:score item-2))]
    (if (item/reverse-sort? item-1)
      (neg? cmp)
      (pos? cmp))))

(defn nearest
  [candidates new-element]
  (first (sort-by #(item/dist % new-element) candidates)))

(defn neighborhood
  "Returns edges of provided item as a list with
  score metric from query-item added to each"
  [layer item query-item visited]
  (->> (get layer (item/id item))
       (item/edges)
       (remove #(contains? visited %))
       (map #(get layer %))
       (map #(add-score % query-item))))

(defn trim-nearest
  "Trims nearest list to n elements while
  retaining the sorted set."
  [n nearest]
  ;; TODO - see if avl/split-key or avl/split-at is any faster
  (into (empty nearest) (take n nearest)))

(defn remove-visited
  "Removes any neighbors that have already been visited
  such that they don't get added to the candidate list."
  [neighbors visited]
  (remove #(contains? visited (item/id %)) neighbors))

(defn search-layer
  "Returns the closest neighbors to the query-item
  in the given layer, starting at the provided entrance-point item."
  [layer query-item entrance-point limit]
  (let [ep (-> (get layer (item/id entrance-point)) ;; get same entrance point in the search layer
               (add-score query-item))] ;; all items in search have distances added from query-item
    (loop [nearest (score-sorted-set ep) ;; return value, nearest list of items to provided query-item
           cands   (score-sorted-set ep) ;; candidate items, 'neighbors' will be added to this set at each evaluation point.
           visited #{}] ;; item ids of visited nodes
      (let [next-c (nearest-s cands)
            done?  (or (nil? next-c)
                       (and
                        (>= (count nearest) limit)
                        ;; check if last item to be returned is farther than the closest candidate
                        (farther? next-c (farthest-s nearest))))]
        (if done?
          (take limit nearest)
          (let [neighbors (neighborhood layer next-c query-item visited)
                cands*    (-> cands
                              (disj next-c)
                              (into neighbors))
                nearest*  (->> (into nearest neighbors)
                               (trim-nearest limit))
                visited*  (into visited (map item/id neighbors))]
            (recur nearest* cands* visited*)))))))

(defn entrance-point
  "Returns (closest match) to target-item for use as
  an entrance point in for-layer.

  The entrance point will be the closest item to the target-item
  in the layer above 'for-layer' (dec for-layer)."
  [db target-item for-layer]
  (reduce
   (fn [ep layer-i]
     (let [layer (db/get-layer db layer-i)
           ep*   (first (search-layer layer target-item ep 1))]
       ep*))
   (db/entry-point db) ;; top level entrance point
   (range (db/top-layer db)
          for-layer
          -1)))

(defn search
  [db query-vector limit]
  (let [query-item (item/create {:values query-vector} (db/metric db) (db/t db))
        ep         (entrance-point db query-item 0) ;; get ep from layer 1 for use on layer 0
        l0         (db/get-layer db 0)]
    (search-layer l0 query-item ep limit)))
