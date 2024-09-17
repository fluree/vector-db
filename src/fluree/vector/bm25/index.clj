(ns fluree.vector.bm25.index
  (:require [fluree.vector.bm25.encode :as encode]))

(defn vectorize-item
  "Vectorizes an item's parsed term frequency
  based on the terms in the bm25 model"
  [terms term-freq]
  (->> term-freq
       (reduce-kv
        (fn [acc term frequency]
          (if-let [idx (get-in terms [term :idx])] ;; indexed items will always have an index, but queries will not
            (conj acc [idx frequency])
            acc))
        [])
       (sort-by first)))

(defn update-avg-len
  [avg-length item-count doc-len]
  (let [total-len   (* avg-length item-count)
        total-len*  (+ doc-len total-len)
        item-count* (inc item-count)
        avg-len*    (/ total-len* item-count*)]
    [avg-len* item-count*]))

(defn- extract-text
  "Takes an item and returns full concatenated text"
  [item]
  (->> (dissoc item "@id")
       vals
       (reduce
        (fn [all-text sentence]
          (if (sequential? sentence)
            (apply str all-text " " sentence)
            (str all-text " " sentence))))))

(defn update-terms
  "Updates index's terms map with the new item's distinct terms

  Returns [terms dimensions]

  As we add new terms, we increase the dimensions accordingly."
  [terms dimensions id terms-distinct]
  (loop [[next-term & r] terms-distinct
         terms      terms
         dimensions dimensions]
    (if next-term
      (let [existing    (get terms next-term)
            term-map    (if existing
                          (update existing :items conj id)
                          {:idx   dimensions ;; sparse vector index location
                           :items #{id}})
            dimensions* (if existing
                          dimensions
                          (inc dimensions))]
        (recur r (assoc terms next-term term-map) dimensions*))
      [terms dimensions])))

(defn index-item
  [{:keys [stemmer stopwords avg-length item-count terms dimensions] :as bm25} item]
  (let [id             (get item "@id")
        item-terms     (-> (extract-text item)
                           (encode/parse-sentence stemmer stopwords))
        doc-len        (count item-terms)
        [avg-length* item-count*] (update-avg-len avg-length item-count doc-len)
        term-freq      (frequencies item-terms)
        terms-distinct (keys term-freq)
        [terms* dimensions*] (update-terms terms dimensions id terms-distinct)
        item-vec       (vectorize-item terms* term-freq)]
    (-> bm25
        (assoc :terms terms*
               :dimensions dimensions*
               :avg-length avg-length*
               :item-count item-count*)
        (assoc-in [:vectors id] item-vec))))

(defn assert-items
  [bm25 assertions]
  (loop [[item & r] assertions
         bm25* bm25]
    (if item
      (recur r (index-item bm25* item))
      bm25*)))

(defn retract-terms-docs
  "Returns updated terms map with doc-id for sparce vec removed"
  [terms id sparse-vec]
  ;; set of term indexes as set we'll disj until empty
  (let [retract-idxs (reduce #(conj %1 (first %2)) #{} sparse-vec)]
    ;; iterate over terms until we retract all items
    (loop [[[term-str term-map] & r] terms
           retract-idxs retract-idxs
           terms        (transient terms)]
      (if (retract-idxs (:idx term-map)) ;; matches one of our retraction items?
        (let [retract-idxs* (disj retract-idxs (:idx term-map))
              terms*        (assoc! terms term-str (update term-map :items disj id))]
          (if (empty? retract-idxs*) ;; no more restriction items, return updated terms map
            (persistent! terms*)
            (recur r retract-idxs* terms*)))
        (recur r retract-idxs terms)))))

(defn- retract-item
  [{:keys [avg-length item-count vectors terms] :as bm25} item]
  (let [id          (get item "@id")
        v           (get vectors id)
        terms*      (retract-terms-docs terms id v)
        vectors*    (dissoc vectors id)
        doc-len     (reduce
                     (fn [acc vec-tuple]
                       (+ acc (second vec-tuple)))
                     0
                     v)
        total-len   (* avg-length item-count)
        total-len*  (- total-len doc-len)
        item-count* (dec item-count)
        avg-length* (/ total-len* item-count*)]
    (assoc bm25 :item-count item-count*
                :avg-length avg-length*
                :vectors vectors*
                :terms terms*)))

(defn retract-items
  [bm25 retractions]
  (loop [[item & r] retractions
         bm25* bm25]
    (if item
      (recur r (retract-item bm25* item))
      bm25*)))
