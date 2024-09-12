(ns fluree.vector.bm25.stop-words
  (:require [clojure.string :as str]))


(defn stopwords
  "Returns a fn that will return truthy if the word is a stopword"
  [lang]
  (let [filename (str "resources/stopwords-" (str/lower-case lang) ".txt")
        data     (slurp filename)
        tokens (set
                (str/split data #"[\n\r\s]+"))]
    (fn [word]
      (tokens word))))

