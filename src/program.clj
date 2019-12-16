(ns program)
(require '[clojure.string :as str])

(defn run-cmd [data pos]
  (defn apply-cmd [op]
    [true (assoc data (data (+ 3 pos)) (op (data (data (+ 1 pos))) (data (data (+ 2 pos)))))])
  (cond (= (data pos) 1)
         (apply-cmd +)
         (= (data pos) 2)
         (apply-cmd *)
        :else
         [false data]))

(defn run-program [data pos]
  (let [[do-cont updated] (run-cmd data pos)]
      (if do-cont
        (recur updated (+ pos 4))
        (updated 0))))

(defn find-nv [input]
  (defn apply-nv [noun verb]
    (assoc input 1 noun 2 verb))
  (first (filter (fn [[n v]] (= (run-program (apply-nv n v) 0) 19690720))
          (for [noun (range 100) verb (range 100)] [noun verb]))))


(let [input (mapv #(Integer/valueOf %) (str/split (str/trim-newline (slurp "data/input02.txt")) #","))]
     (find-nv input))