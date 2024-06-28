(ns fluree.hnsw.index
  (:refer-clojure :exclude [empty?]))

(set! *warn-on-reflection* true)

(defn max-layers
  [index]
  (:max-layers index))

(defn entrace-point-atom
  [index]
  (:eP index))

(defn update-entrance-point
  [index entry-point]
  (reset! (entrace-point-atom index) entry-point)
  index)

(defn state
  "Returns state atom for index"
  [index]
  (:state index))

(defn layers-atom
  [index]
  (:layers index))


(defn update-layers
  [index layers]
  (reset! (layers-atom index) layers)
  index)

(defn update!
  ([index layers]
   (update-layers index layers))
  ([index entry-point layers]
   (-> index
       (update-layers layers)
       (update-entrance-point entry-point))))


(defn initialize-layers
  [layers-n]
  (mapv (constantly {}) (range layers-n)))

(defn blank-db
  [{:keys [dimensions max-layers metric max-k level-factor efConstruction] :as options}]
  (let []
    {:t               0
     :layers          (initialize-layers max-layers)
     :top-layer       (dec max-layers)
     :eP              nil
     :dimensions      dimensions
     :max-layers      max-layers
     :max-m0          (* max-layers 2)
     :max-k           max-k
     :metric metric
     :level-factor    level-factor
     :efConstruction  efConstruction}))

(defn create
  [{:keys [dimensions max-layers metric max-k level-factor efConstruction] :as options}]
  (let []
    {:efConstruction  efConstruction
     :state           (atom {:db (blank-db options)})
     :eP              (atom nil) ;; entrance point
     :layers          (atom (initialize-layers max-layers))
     :level-factor    level-factor ;; factor for level generation (~ how many more * items in each successive level)
     :max-k           max-k ;; max edges per node per layer
     :metric metric ;; similarity metric - :dotproduct, :cosine, :distance
     :dimensions      dimensions ;; vector dimensions - 'count' of vector
     :max-layers      max-layers ;; max layers in hierarchy
     :max-m0          (* max-layers 2)}))

(defn db
  [index]
  (:db @(state index)))
