(ns fluree.vector.hnsw.layer
  (:require [clojure.math :as math]
            [fluree.vector.hnsw.item :as item]))

(set! *warn-on-reflection* true)

(defn layer-lambda
  [max-layers]
  (/ 1 (math/log max-layers)))

(defn assign-hash
  "Uses hash to assign a layer deterministically"
  [item-id layer-lambda topL]
  (let [hash   (hash item-id)
        random (double (/ (abs hash) (Integer/MAX_VALUE)))]
    (min
     topL
     (int (* (- (math/log random))
             layer-lambda)))))

(defn assign-rand
  "Randomly assigns a layer level to a new item based on
  the logrithmic distribution represented by level-factor,
  where each successive level will have `level-factor` times
  more items.

  level-factor must be > 1.0, and is probably in the range of
   4 +/-. A level factor of 10 would need billions of items
   across 8+/- levels to have a chance of decent distribution."
  [level-factor max-levels]
  (- ;; algo returns numbers weighted towards the top of the range, so subtract max-levels to weight to the bottom (zero index)
   (dec max-levels) ;; levels are zero-indexed, so decrease max # of levels by 1
   (int ;; round down to a whole number which will be a layer index
    (/
     (math/log
      (+ 1.0
         (* (math/random)
            (- (math/pow level-factor max-levels) 1.0))))
     (math/log level-factor)))))

(defn item
  "Retrieves an item/node from a layer of provided id"
  [layer id]
  (get layer id))

(defn get-layer
  "Returns layer of index 'i' from the layers map"
  [layers i]
  (get layers i))

(defn update-layer
  [layers i layer]
  (assoc layers i layer))

(defn initialize
  "Creates a new layer with an initial item/vector"
  [initial-item]
  (assoc {} (item/id initial-item) initial-item))


