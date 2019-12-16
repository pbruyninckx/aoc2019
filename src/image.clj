(ns image
  (:require [clojure.string :as str]))

(def raw-input-data (str/trim-newline (slurp "data/input08.txt")))

(defn char-to-int [c] (- (int c) (int \0)))

(def input-data (map char-to-int raw-input-data))

(def width 25)
(def height 6)
(def area (* 25 6))

(def layers (partition area input-data))

(let [num-count (map frequencies layers)
      sought-count (apply min-key #(% 0) num-count)]
  (* (sought-count 1) (sought-count 2)))

(defn first-colour [c] (first (filter #(not= 2 %) c)))

(def test-layers [[0 2 2 2] [1 1 2 2] [2 2 1 2] [0 0 0 0]])

(let [image (map first-colour (apply map list layers))
      image-by-row (partition 25 image)]
  (doseq [image-row image-by-row]
    (println image-row)))