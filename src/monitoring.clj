(ns monitoring
  (:require [clojure.string :as str]))

(def raw-input (str/split-lines (slurp "data/input10.txt")))

(defn find-x-coords [line]
  (mapv #(% 0)
        (filter #(= (% 1) \#)
                (map-indexed vector line))))

(def coords
  (reduce #(into %1 %2) []
          (map #(map vector (% 1) (repeat (% 0)))
               (map-indexed vector (map find-x-coords raw-input)))))

(defn gcd
  "(gcd a b) computes the greatest common divisor of a and b."
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn simplify-fraction [a b]
  (if (= 0 a b)
    [0 0]
    (let [g (Math/abs (gcd a b))]
      (mapv #(/ % g) [a b]))))

(def num-visible
  (mapv (fn [outer-coord]
          (count (into #{} (map
                             (fn [coord] (apply simplify-fraction (map - outer-coord coord)))
                             coords))))
        coords))

(dec (apply max num-visible))

(def station-ind ((first (filter #(= (% 1) 310) (map-indexed vector num-visible))) 0))
(def station-coord (coords station-ind))

(defn coords-to-order [c]
  (- (Math/atan2 (c 0) (c 1))))

(def rel-coords
  (filter #(not (= 0 (% 0) (% 1)))
          (map (fn [coord] (mapv - coord station-coord)) coords)))
(def angles (map coords-to-order rel-coords))

(def sorted-angles (sort (into #{} angles)))

(defn abs-sum [c]
  (reduce + (map #(Math/abs %) c)))

(def coords-by-angle
  (let [empty-map (into {} (map #(vector % []) sorted-angles))
        unsorted-map (reduce
                       (fn [coll [angle coord]]
                         (update coll angle #(conj % coord)))
                       empty-map
                       (map vector angles rel-coords))]
    (into [] (sort
               (map
               (fn [[k v]] [k (sort-by abs-sum v)])
               unsorted-map)))))

(defn take-1 [coords-by-angle pos]
  (defn inc-pos [p]
    (if (< p (dec (count coords-by-angle)))
      (inc p)
      0))
  (let [pos-coords ((coords-by-angle pos) 1)]
    (if (empty? pos-coords)
      (recur coords-by-angle (inc-pos pos))
      [(first pos-coords) (update-in coords-by-angle [pos 1] rest) (inc-pos pos)])))

(defn take-n [coords-by-angle n pos]
  (if (= n 1)
    ((take-1 coords-by-angle pos) 0)
    (let [[_ new-coords new-pos] (take-1 coords-by-angle pos)]
      (recur new-coords (dec n) new-pos))))

(map + (take-n coords-by-angle 200 0) station-coord)