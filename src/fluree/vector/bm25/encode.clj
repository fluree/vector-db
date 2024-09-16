(ns fluree.vector.bm25.encode
  (:require [clojure.string :as str]
            [fluree.vector.bm25.stemmer :as stm]))

(def SPACE_PATTERN #"[^\w]+")

(defn- split-text
  [text]
  (str/split (str/lower-case text) SPACE_PATTERN))

(defn parse-sentence
  [sentence stemmer stopwords]
  (let [xf (comp
            (remove stopwords)
            (map #(stm/stem stemmer %)))]
    (->> sentence
         (split-text)
         (sequence xf))))