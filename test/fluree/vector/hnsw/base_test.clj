(ns fluree.vector.hnsw.base-test
  (:require [clojure.test :refer :all]
            [fluree.vector.hnsw.test-utils :as test-utils]
            [fluree.vector.hnsw :as hnsw]))


(deftest recall-test
  (testing "HNSW index has expected recall rates."
    (let [article-vecs (vec (test-utils/small-articles-vectors))
          index        (hnsw/create {:max-layers     4
                                     :metric         :dotproduct
                                     :max-k          16
                                     :level-factor   4
                                     :efConstruction 100})
          db           (hnsw/upsert index (take 100 article-vecs))]

      (testing " have 100% recall with result size of 5"
        (let [samples       10 ;; creates db this many times to measure recall
              dbs           (take samples (repeatedly (fn [] (hnsw/upsert index (take 100 article-vecs)))))
              top-k         5
              query         {:vector (get test-utils/query-embeddings "AI use in digital devices")
                             :top-k  top-k}
              flat-rank-res (test-utils/flat-rank db query)
              ;; get a recall number for every db
              recall        (mapv (fn [db]
                                    (let [query-res (hnsw/search db query)
                                          recall    (test-utils/measure-recall flat-rank-res query-res)]
                                      recall))
                                  dbs)
              recall-avg    (float (/ (reduce + recall) samples))]
          (is (= recall-avg (float 1.0))
              "Recall rate is 100%"))))))


(deftest large-vector-insertion-test
  (testing "Inserting a large number of vectors into the HNSW index."
    (let [article-vecs  (test-utils/large-articles-vectors 2500)
          index         (hnsw/create {:max-layers     6
                                      :metric         :dotproduct
                                      :max-k          16
                                      :level-factor   4
                                      :efConstruction 100})
          db            (hnsw/upsert index article-vecs)
          query         {:vector (get test-utils/query-embeddings "AI use in digital devices")
                         :top-k  100}
          search-res    (time (hnsw/search db query))
          flat-rank-res (test-utils/flat-rank db query)
          recall        (test-utils/measure-recall flat-rank-res search-res)]
      (println "recall: " recall)

      (is (> recall (float 0.9))
          "Recall should be relatively high with larger vector sets."))))
