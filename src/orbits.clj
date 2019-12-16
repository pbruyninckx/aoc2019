(ns orbits)

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def test-string "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN")

(defn num-orbits [all-orbits element]
  (loop [element element
         ret 0]
    (if (or (= element "COM") (nil? element))
      ret
      (let [parent (all-orbits element)]
        (recur parent (+ ret 1))))))

(defn elements-to-root [all-orbits element]
  (loop [element element
         ret '()]
    (if (or (= element "COM") (nil? element))
      (set ret)
      (let [parent (all-orbits element)]
        (recur parent (conj ret parent))))))

(defn get-distance [all-orbits el-a el-b]
  (let [to-root-a (elements-to-root all-orbits el-a)
        to-root-b (elements-to-root all-orbits el-b)
        num-common (count (set/intersection to-root-a to-root-b))]
    (assert (and (not (contains? to-root-a el-b))
                 (not (contains? to-root-b el-a))))
    (+ (count to-root-a) (count to-root-b) (- (* 2 num-common)) 2)))

(def test-orbits (into {} (map (comp vec reverse #(str/split % #"\)"))
                               (str/split-lines test-string))))

(with-open [rdr (clojure.java.io/reader "data/input06.txt")]
  (let [orbits (into {} (map (comp vec reverse #(str/split % #"\)"))
                             ;(str/split-lines test-string)
                             (line-seq rdr)
                             ))]
    ;(println (reduce + (map #(num-orbits orbits %) (keys orbits))))))
    (println (get-distance orbits (orbits "SAN") (orbits "YOU")))))

