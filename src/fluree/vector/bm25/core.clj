(ns fluree.vector.bm25.core
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [fluree.vector.bm25.stemmer :as stm]
            [fluree.vector.bm25.stop-words :as sw]))

;; https://en.wikipedia.org/wiki/Okapi_BM25

(def SPACE_PATTERN #"[^\w]+")

(defn- split-text
  [text]
  (str/split (str/lower-case text) SPACE_PATTERN))

(defn- parse-sentence
  [sentence stemmer stopwords]
  (let [xf (comp
            (remove stopwords)
            (map #(stm/stem stemmer %)))]
    (->> sentence
         (split-text)
         (sequence xf))))

;; TODO - this can be pre-calculated
(defn get-doc-length
  "Return document length for doc-id of corpus"
  [vector]
  (->> vector
       (map second)
       (reduce +)))

(defn initialize
  ([] (initialize nil))
  ([{:keys [k1 b lang weights] :or {k1 1.2 b 0.75 lang "en"}}]
   (let [stemmer   (stm/initialize)
         stopwords (sw/stopwords lang)]
     {:k1         k1
      :b          b
      :vectors    {}
      :dimensions 0
      :item-count 0
      :avg-length 0
      :terms      {} ;; holds term as keys, term idx, term idf and item-map as vals
      :stemmer    stemmer
      :stopwords  stopwords
      :weights    weights})))

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
                           :idf   nil ;; will be filled in after all items are indexed
                           :items #{id}})
            dimensions* (if existing
                          dimensions
                          (inc dimensions))]
        (recur r (assoc terms next-term term-map) dimensions*))
      [terms dimensions])))

(defn update-avg-len
  [avg-length item-count doc-len]
  (let [total-len   (* avg-length item-count)
        total-len*  (+ doc-len total-len)
        item-count* (inc item-count)
        avg-len*    (/ total-len* item-count*)]
    [avg-len* item-count*]))

(defn update-idf
  [item-count terms]
  (let [terms*
        (reduce-kv
         (fn [acc term {:keys [items] :as term-map}]
           (let [idf* (Math/log
                       (+ (/ (+ (- item-count (count items)) 0.5)
                             (+ (count items) 0.5))
                          1))]
             (assoc acc term (assoc term-map :idf idf*))))
         terms
         terms)]
    terms*))

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

(defn index-item
  [{:keys [stemmer stopwords avg-length item-count terms dimensions] :as bm25} item]
  (let [id             (get item "@id")
        item-terms     (-> (extract-text item)
                           (parse-sentence stemmer stopwords))
        doc-len        (count item-terms)
        [avg-length* item-count*] (update-avg-len avg-length item-count doc-len)
        term-freq      (frequencies item-terms)
        terms-distinct (keys term-freq)
        [terms* dimensions*] (update-terms terms dimensions id terms-distinct)
        terms**        (update-idf item-count* terms*)
        item-vec       (vectorize-item terms** term-freq)]
    (-> bm25
        (assoc :terms terms**
               :dimensions dimensions*
               :avg-length avg-length*
               :item-count item-count*)
        (assoc-in [:vectors id] item-vec))))

(defn index-items
  [bm25 items]
  (loop [[item & r] items
         bm25* bm25]
    (if item
      (recur r (index-item bm25* item))
      bm25*)))

(defn seek-term
  "Returns nil once doc-vec is exhausted"
  [term doc-vec]
  (loop [doc-vec* doc-vec]
    ;; term-tuple is a two-tuple of [term-idx term-frequency]
    (when-let [term-tuple (first doc-vec*)]
      (let [term-idx (first term-tuple)]
        (cond
          ;; matching term, return rest of doc-vec and term frequency
          (= term term-idx)
          [(rest doc-vec) (second term-tuple)]

          ;; have passed matching term, return vec to that point
          ;; no term-freq as no match
          (> term-idx term)
          [doc-vec*]

          :else
          (recur (rest doc-vec*)))))))

(defn calc-term-score
  [k1 b avg-doc-len doc-len term-f]
  (/ (* term-f (+ k1 1))
     (+ term-f (* k1 (+ (- 1 b) (* b (/ doc-len
                                        avg-doc-len)))))))

(defn calc-doc-score
  [k1 b avg-length query-vec doc-vec]
  (let [doc-len (get-doc-length doc-vec)]
    (loop [[query-term & r] query-vec
           doc-vec doc-vec
           score   0.0]
      (if query-term
        (let [[doc-vec* term-f] (seek-term query-term doc-vec)
              score* (if term-f
                       (+ score
                          (calc-term-score k1 b avg-length doc-len term-f))
                       score)]
          (if doc-vec*
            (recur r doc-vec* score*)
            score*))
        score))))

(defn search
  [{:keys [stemmer stopwords vectors k1 b avg-length terms] :as bm25} query]
  (let [query-vec (->> (parse-sentence query stemmer stopwords)
                       (distinct)
                       ;; TODO: should be able to remove frequencies below, with distinct they will all be '1'
                       (frequencies)
                       (vectorize-item terms)
                       ;; TODO - once 'vectorize' item is done for query-specific, below simulates us building the vector of just terms in the query, no frequencies
                       (map first))]
    (->> vectors
         (reduce-kv
          (fn [acc doc-id doc-vec]
            (let [score (calc-doc-score k1 b avg-length query-vec doc-vec)]
              (conj acc [doc-id score])))
          [])
         (sort-by second #(compare %2 %1)))))



