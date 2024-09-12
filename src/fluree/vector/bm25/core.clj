(ns fluree.vector.bm25.core
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [fluree.vector.bm25.stemmer :as stm]
            [fluree.vector.bm25.stop-words :as sw]))

;; https://en.wikipedia.org/wiki/Okapi_BM25

(def SPACE_PATTERN #"\s+")

(defn- parse-sentence
  [sentence stemmer stopwords]
  (let [xf    (comp
               (remove stopwords)
               (map #(do (println "- " %) %))
               (map #(stm/stem stemmer %)))
        terms (str/split (str/lower-case sentence) SPACE_PATTERN)]
    (sequence xf terms)))

(defn doc-frequency-calc
  [corpus stemmer stopwords]
  (->> corpus
       (map-indexed
        (fn [idx doc]
          (->> (str/split (str/lower-case doc) SPACE_PATTERN)
               (remove stopwords)
               (distinct)
               (map (fn [word] [(stm/stem stemmer word) #{idx}]))
               (into {}))))
       (apply merge-with set/union)))

(defn term-frequency-calc
  [corpus stemmer stopwords]
  (->> corpus
       (map-indexed
        (fn [idx doc]
          [idx
           (->> (str/split (str/lower-case doc) SPACE_PATTERN)
                (remove stopwords)
                (map #(stm/stem stemmer %))
                (frequencies))]))
       (into {})))

(defn idf-calc
  "Calculate inverse document frequency (idf)"
  [corpus doc-frequency]
  (let [corpus-size (count corpus)]
    (->> doc-frequency
         (map
          (fn [[term doc-ids]]
            [term
             (Math/log
              (+ (/ (+ (- corpus-size (count doc-ids)) 0.5)
                    (+ (count doc-ids) 0.5))
                 1))]))
         (into {}))))

(defn calc-avg-doc-length
  [corpus]
  (if (seq corpus)
    (double
     (/ (->> corpus
             (map #(count (str/split % SPACE_PATTERN)))
             (reduce +))
        (count corpus)))
    0))

(defn calc-term-score
  [{:keys [tf idf k1 b avg-doc-len] :as bm-25} term doc-id doc-len]
  (let [term-f  (get-in tf [doc-id term] 0)
        idf-val (get idf term 0.0)]
    (if idf-val
      (/ (* term-f (+ k1 1))
         (+ term-f (* k1 (+ (- 1 b) (* b (/ doc-len
                                            avg-doc-len))))))
      0.0)))

;; TODO - this can be pre-calculated
(defn get-doc-length
  "Return document length for doc-id of corpus"
  [{:keys [tf] :as _bm-25} doc-id]
  (->> (get tf doc-id)
       vals
       (reduce +)))

(defn calc-doc-score
  [bm-25 q doc-id]
  (let [doc-len (get-doc-length bm-25 doc-id)]
    (->> q
         distinct
         (map #(calc-term-score bm-25 % doc-id doc-len))
         (reduce +))))

(defn search
  [{:keys [tf] :as bm-25} query]
  (let [q       (parse-sentence query (:stemmer bm-25) (:stopwords bm-25))
        _       (println "parsed-query: " q)
        doc-ids (keys tf)]
    (->> doc-ids
         (map #(vector % (calc-doc-score bm-25 q %)))
         (sort-by second #(compare %2 %1)))))

(defn bm-25
  [corpus {:keys [k1 b] :or {k1 1.2 b 0.75}}]
  (let [stemmer     (stm/initialize)
        stopwords   (sw/stopwords "en")
        df          (doc-frequency-calc corpus stemmer stopwords)
        tf          (term-frequency-calc corpus stemmer stopwords)
        idf         (idf-calc corpus df)
        avg-doc-len (calc-avg-doc-length corpus)]
    {:k1          k1
     :b           b
     :avg-doc-len avg-doc-len
     :tf          tf
     :idf         idf
     :df          df
     :stemmer     stemmer
     :stopwords   stopwords}))


