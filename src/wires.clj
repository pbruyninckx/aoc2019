(ns wires)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn parse-wire-data [wire-string]
  (defn parse-single-line [single-string]
    [(first single-string) (-> single-string (subs 1) (Integer/parseInt))])
  (map parse-single-line (str/split wire-string #",")))

(defn get-single-wire-positions [[[x0 y0] l] [direction length]]
  (mapv vector
        (case direction
          \R (for [x (range (inc x0) (+ x0 length 1))]
               [x y0])
          \L (for [x (range (dec x0) (- x0 length 1) -1)]
               [x y0])
          \U (for [y (range (inc y0) (+ y0 length 1))]
               [x0 y])
          \D (for [y (range (dec y0) (- y0 length 1) -1)]
               [x0 y]))
        (range (inc l) (+ l length 1))))

(defn get-wire-positions [wires]
  (into [] (reduce (fn [existing-path single-line]
                     (into existing-path (get-single-wire-positions (last existing-path) single-line)))
                   [[[0 0] 0]]
                   wires)))

(with-open [rdr (clojure.java.io/reader "data/input03.txt")]
  (let [wires (map (comp get-wire-positions parse-wire-data) (line-seq rdr))
        wire-pos-sets (map #(into #{} (map first %)) wires)
        pos-to-num (map #(into {} %) wires)
        joints (disj (apply set/intersection wire-pos-sets) [0 0])]
    (apply min (map
                 (fn [joint] (apply + (map #(% joint) pos-to-num)))
                 joints))))