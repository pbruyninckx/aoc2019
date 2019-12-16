(ns rocket)

(defn fuel-needed [mass]
  (-> mass (Math/floorDiv 3) (- 2)))

(defn recursive-fuel [mass]
  (loop [mass mass ret 0]
    (if (<= mass 0)
      ret
      (let [more-fuel (fuel-needed mass)]
        (if (>= more-fuel 0)
          (recur more-fuel (+ more-fuel ret))
          ret)))))

(with-open [rdr (clojure.java.io/reader "data/input01.txt")]
  (reduce + (map #(-> % Integer/parseInt fuel-needed) (line-seq rdr))))

(with-open [rdr (clojure.java.io/reader "data/input01.txt")]
  (reduce + (map #(-> % Integer/parseInt recursive-fuel) (line-seq rdr))))
