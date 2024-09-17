(ns fluree.vector.bm25.core
  (:require [fluree.vector.bm25.stemmer :as stm]
            [fluree.vector.bm25.stop-words :as sw]
            [fluree.vector.bm25.search :as search]
            [fluree.vector.bm25.index :as index])
  (:refer-clojure :exclude [update]))

;; https://en.wikipedia.org/wiki/Okapi_BM25

(defn update
  ([bm25 assertions]
   (update bm25 assertions nil))
  ([bm25 assertions retractions]
   (-> bm25
       (index/retract-items retractions)
       (index/assert-items assertions))))

(defn search
  [bm25 query]
  (search/search bm25 query))

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









