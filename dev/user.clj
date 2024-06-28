(ns user
  (:require [fluree.hnsw :as hnsw]
            [fluree.hnsw.item :as item]
            [fluree.hnsw.layer :as layer]
            [fluree.hnsw.db :as db]
            [clojure.data.json :as json]
            [mikera.vectorz.core :as v]
            [djl.embedding :as embed]))


(comment

 (def model (embed/model "djl://ai.djl.huggingface.pytorch/sentence-transformers/all-MiniLM-L6-v2"))
 #_(def model (embed/model "djl://ai.djl.huggingface.pytorch/sentence-transformers/all-mpnet-base-v2"))

 (vec (embed/embedding model "AI use in digital devices"))

 )