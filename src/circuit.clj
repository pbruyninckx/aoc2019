(ns circuit)

(require '[clojure.string :as str])

(defrecord instruction [opcode modes])

(defn split-instruction [instruction-code]
  (defn split-modes [modes]
    (loop [modes modes ret []]
      (if (= modes 0)
        (apply conj ret (repeat (- 2 (count ret)) :position))
        (let [mode (if (= 1 (rem modes 10)) :immediate :position)
              rest-modes (Math/floorDiv modes 10)]
          (recur rest-modes (conj ret mode))))))
  (instruction. (rem instruction-code 100) (split-modes (Math/floorDiv instruction-code 100))))

(defrecord cmd-output [continue data output input pos])

(defn run-cmd [data input pos]
  (defn get-data [offset mode]
    (case mode
      :immediate (data (+ offset pos))
      :position (data (data (+ offset pos)))))
  (defn apply-cmd [instruction]
    (let [op (if (= (:opcode instruction) 1) + *)
          modes (:modes instruction)
          new-data (assoc data (get-data 3 :immediate) (op (get-data 1 (modes 0))
                                                           (get-data 2 (modes 1))))]
      (cmd-output. true new-data nil input (+ pos 4))))
  (let [instruction (split-instruction (get-data 0 :immediate))
        modes (:modes instruction)]
    (case (:opcode instruction)
      (1 2) (apply-cmd instruction)
      3 (cmd-output. true (assoc data (get-data 1 :immediate) (first input)) nil (rest input) (+ pos 2))
      4 (cmd-output. true data (get-data 1 ((:modes instruction) 0)) input (+ pos 2))
      5 (cmd-output. true data nil input (if (not= 0 (get-data 1 (modes 0))) (get-data 2 (modes 1)) (+ pos 3)))
      ;'jump-if-true
      6 (cmd-output. true data nil input (if (= 0 (get-data 1 (modes 0))) (get-data 2 (modes 1)) (+ pos 3)))
      ; jump-if-false
      7 (cmd-output. true (assoc data (get-data 3 :immediate)
                                      (if (< (get-data 1 (modes 0)) (get-data 2 (modes 1))) 1 0))
                     nil input (+ pos 4))
      ; 'less-than
      8 (cmd-output. true (assoc data (get-data 3 :immediate)
                                      (if (= (get-data 1 (modes 0)) (get-data 2 (modes 1))) 1 0))
                     nil input (+ pos 4))
      ; 'equals
      99 (cmd-output. false data nil input pos))))

(defn run-program [data input pos]
  (let [cmd-output (run-cmd data input pos)]
    (if (:output cmd-output)
      cmd-output
      (if (:continue cmd-output)
        (recur (:data cmd-output) (:input cmd-output) (:pos cmd-output))
        cmd-output))))

(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s]
                      (map #(cons x %) (permutations (remove #{x} s)))))
      [s])))

(defn run-circuit [data config]
  (reduce (fn [prev setting] (:output (run-program data (list setting prev) 0))) 0 config))

(defn run-circular-circuit [data config]
  (let [amplifiers (vec (drop 1 (reduce (fn [prevs setting]
                                          (conj prevs (run-program data (list setting (:output (last prevs))) 0)))
                                        [{:output 0}] config)))]
    (loop [amplifiers amplifiers current-amplifier-index 0]
      (let [prev-amplifier (amplifiers (mod (dec current-amplifier-index) 5))
            next-amplifier-ind (mod (inc current-amplifier-index) 5)
            current-amplifier (amplifiers current-amplifier-index)
            updated-amplifier (run-program (:data current-amplifier)
                                           (list (:output prev-amplifier))
                                           (:pos current-amplifier))]
        (if (not (:continue updated-amplifier))
          (if (= 4 current-amplifier-index)
            (:output updated-amplifier)
            (:output (amplifiers 4)))
          (recur (assoc amplifiers current-amplifier-index updated-amplifier)
                 next-amplifier-ind))))))


(let [input (mapv #(Integer/valueOf %) (str/split (str/trim-newline (slurp "data/input07.txt")) #","))]
  (apply max (map #(run-circuit input %) (permutations (range 5)))))

(let [input (mapv #(Integer/valueOf %) (str/split (str/trim-newline (slurp "data/input07.txt")) #","))]
  (apply max (map #(run-circular-circuit input %) (permutations (range 5 10)))))