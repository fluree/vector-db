(ns fluree.vector.bm25.stemmer
  (:import (org.tartarus.snowball SnowballStemmer)
           (org.tartarus.snowball.ext englishStemmer)))

(defn stem
  [stemmer word]
  (doto stemmer
    (.setCurrent word)
    (.stem))
  (.getCurrent stemmer))

(defn initialize
  []
  (englishStemmer.))

