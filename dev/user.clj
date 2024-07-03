(ns user
  (:require [fluree.hnsw :as hnsw]
            [fluree.hnsw.item :as item]
            [fluree.hnsw.layer :as layer]
            [fluree.hnsw.db :as db]
            [mikera.vectorz.core :as v]
            [fluree.hnsw.test-utils :as test-utils]
            [criterium.core :as criterium]
            [djl.embedding :as embed]))

(def model (embed/model "djl://ai.djl.huggingface.pytorch/sentence-transformers/all-MiniLM-L6-v2"))

(comment

 (def article-vecs (test-utils/small-articles-vectors))
 (def articles (test-utils/small-articles-dataset))
 (def article-lookup (zipmap (map :id articles) articles))

 (first article-vecs)

 (def index (hnsw/create {:max-layers     4
                          :metric         :dotproduct
                          :max-k          16
                          :level-factor   4
                          :efConstruction 32}))

 (def db (hnsw/upsert index article-vecs))


 (hnsw/search db {:vector (embed/embedding model "AI and data")
                  :top-k  5})

 ;; include title in the result
 (->> (hnsw/search db {:vector (embed/embedding model "AI and data")
                       :top-k  5})
      (map #(assoc % :title (get-in article-lookup [(:id %) :title]))))

 ;; test recall
 (let [samples       100 ;; creates db this many times to measure recall
       dbs           (take samples (repeatedly (fn [] (hnsw/upsert index article-vecs))))
       top-k         5
       query         {:vector (embed/embedding model "AI and data")
                      :top-k  top-k}
       flat-rank-res (test-utils/flat-rank db query)
       ;; get a recall number for every db
       recall        (mapv (fn [db]
                             (let [query-res (hnsw/search db query)
                                   recall    (test-utils/measure-recall flat-rank-res query-res)]
                               recall))
                           dbs)
       recall-avg    (float (/ (reduce + recall) samples))]
   recall)

 )

(comment

 ;; larger vector insertion
 ;; setup
 (def lg-articles (test-utils/large-articles-dataset 10000 nil))
 (def lg-article-vecs (test-utils/large-articles-vectors 10000))
 (def lg-article-lookup (zipmap (map :id lg-articles) lg-articles))

 ;; create db
 (def lg-index (hnsw/create {:max-layers     6
                             :metric         :dotproduct
                             :max-k          16
                             :level-factor   4
                             :efConstruction 32}))

 (def lg-db (hnsw/upsert index lg-article-vecs))

 (time

  (hnsw/search lg-db {:vector (embed/embedding model "Weather events and global warming")
                      :top-k  10}))

 (->> {:vector (embed/embedding model "Technology and computers")
       :top-k  100}
      (hnsw/search lg-db)
      (take 10)
      (map #(assoc % :title (get-in lg-article-lookup [(:id %) :title]))))

 )






(comment

 (def model (embed/model "djl://ai.djl.huggingface.pytorch/sentence-transformers/all-MiniLM-L6-v2"))
 #_(def model (embed/model "djl://ai.djl.huggingface.pytorch/sentence-transformers/all-mpnet-base-v2"))

 (vec (embed/embedding model "AI use in digital devices"))

 )