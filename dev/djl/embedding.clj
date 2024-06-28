(ns djl.embedding
  (:require [mikera.vectorz.core :as v])
  (:import (ai.djl.huggingface.translator TextEmbeddingTranslatorFactory)
           (ai.djl.repository.zoo Criteria ZooModel ModelZoo)))

(def djl-model "djl://ai.djl.huggingface.pytorch/sentence-transformers/all-MiniLM-L6-v2")

(defn criteria
  [djl-model]
  (let [criteria (Criteria/builder)
        criteria (.setTypes criteria java.lang.String (type (float-array [])))
        criteria (.optModelUrls criteria djl-model)
        criteria (.optEngine criteria "PyTorch")
        criteria (.optTranslatorFactory criteria (TextEmbeddingTranslatorFactory.))]
    (.build criteria)))

(defn model
  "Loads a model ready for prediction"
  [model-uri]
  (let [criteria (criteria model-uri)]
    (.loadModel criteria)))

(defn list-models
  []
  (ModelZoo/listModels))

(defn embedding
  [model text]
  (let [predictor (.newPredictor model)]
    (seq (.predict predictor text))))


(comment

 (list-models)

 (def m (model (criteria djl-model)))

 (time
  (let [v1 (v/vec (seq (embedding m "London is known for its finacial district")))
        v2 (v/vec (seq (embedding m "London has 9,787,426 inhabitants at the 2011 census")))
        v3 (v/vec (seq (embedding m "The United Kingdom is the fourth largest exporter of goods in the world")))

        q  (v/vec (seq (embedding m "How many people are in London")))]

    (println "v1" (v/dot q v1))
    (println "v2" (v/dot q v2))
    (println "v3" (v/dot q v3))))

 )

