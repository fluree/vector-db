(ns fluree.vector.bm25.search
  (:require [fluree.vector.bm25.encode :as encode]))


(defn calc-term-score
  [k1 b avg-doc-len doc-len term-idf term-f]
  (* term-idf
     (/ (* term-f (+ k1 1))
        (+ term-f (* k1 (+ (- 1 b) (* b (/ doc-len
                                           avg-doc-len))))))))

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

;; TODO - this can be pre-calculated
(defn get-doc-length
  "Return document length for doc-id of corpus"
  [vector]
  (->> vector
       (map second)
       (reduce +)))

(defn calc-doc-score
  [k1 b avg-length query-vec doc-vec]
  (let [doc-len (get-doc-length doc-vec)]
    (loop [[[query-term term-idf] & r] query-vec
           doc-vec doc-vec
           score   0.0]
      (if query-term
        (let [[doc-vec* term-f] (seek-term query-term doc-vec)
              score* (if term-f
                       (+ score
                          (calc-term-score k1 b avg-length doc-len term-idf term-f))
                       score)]
          (if doc-vec*
            (recur r doc-vec* score*)
            score*))
        score))))

(defn calc-idf
  [item-count n-instances]
  (Math/log
   (+ 1 (/ (+ (- item-count n-instances) 0.5)
           (+ n-instances 0.5)))))

(defn parse-query
  "Based on query text, returns vector of two-tuples that include
  [term-sparse-vec-index n-times-term-appears-in-items]

  Only returns terms that are in the index."
  [query terms item-count stemmer stopwords]
  (let [q-terms (->> (encode/parse-sentence query stemmer stopwords)
                     (distinct))]
    (reduce
     (fn [acc term]
       (if-let [term-match (get terms term)] ;; won't match term if not in index
         (conj acc [(:idx term-match) (calc-idf item-count (count (:items term-match)))])
         acc))
     []
     q-terms)))

(defn search
  [{:keys [stemmer stopwords item-count vectors k1 b avg-length terms] :as _bm25} query]
  (let [query-vec (parse-query query terms item-count stemmer stopwords)]
    (->> vectors
         (reduce-kv
          (fn [acc doc-id doc-vec]
            (let [score (calc-doc-score k1 b avg-length query-vec doc-vec)]
              (conj acc [doc-id score])))
          [])
         (sort-by second #(compare %2 %1)))))
