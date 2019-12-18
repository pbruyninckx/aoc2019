(ns care-package
  (:require [clojure.string :as str]))

(defrecord instruction [opcode modes])

(defn split-instruction [instruction-code]
  (defn split-modes [modes]
    (loop [modes modes ret []]
      (if (= modes 0)
        (apply conj ret (repeat (- 3 (count ret)) :position))
        (let [mode (case (rem modes 10)
                     0 :position
                     1 :immediate
                     2 :relative)
              rest-modes (Math/floorDiv modes 10)]
          (recur rest-modes (conj ret mode))))))
  (instruction. (rem instruction-code 100) (split-modes (Math/floorDiv instruction-code 100))))

(defrecord machine-state [continue data output input pos rel-base])

(defn run-cmd [state]
  (let [{:keys [data input pos rel-base]} state]
    (defn get-data [offset mode]
      (case mode
        :immediate (get data (+ offset pos) 0)
        :position (get data (get-data offset :immediate) 0)
        :relative (get data (+ rel-base (get-data offset :immediate)) 0)))
    (defn save-data [offset mode val]
      (case mode
        :immediate (throw "Invalid mode")
        :position (assoc data (get-data offset :immediate) val)
        :relative (assoc data (+ (get-data offset :immediate) rel-base) val)))
    (defn apply-cmd [instruction]
      (let [op (if (= (:opcode instruction) 1) + *)
            modes (:modes instruction)
            new-data (save-data 3 (modes 2) (op (get-data 1 (modes 0))
                                                (get-data 2 (modes 1))))]
        (assoc state :data new-data :pos (+ pos 4))))
    (let [instruction (split-instruction (get-data 0 :immediate))
          modes (:modes instruction)]
      (case (:opcode instruction)
        (1 2) (apply-cmd instruction)
        3 (assoc state :data (save-data 1 (modes 0)
                                        (first input)) :input (rest input) :pos (+ pos 2))
        4 (assoc state :output (get-data 1 ((:modes instruction) 0)) :pos (+ pos 2))
        5 (assoc state :pos (if (not= 0 (get-data 1 (modes 0))) (get-data 2 (modes 1)) (+ pos 3)))
        ;'jump-if-true
        6 (assoc state :pos (if (= 0 (get-data 1 (modes 0))) (get-data 2 (modes 1)) (+ pos 3)))
        ; jump-if-false
        7 (assoc state :data (save-data 3 (modes 2)
                                        (if (< (get-data 1 (modes 0)) (get-data 2 (modes 1))) 1 0))
                       :pos (+ pos 4))
        ; 'less-than
        8 (assoc state :data (save-data 3 (modes 2)
                                        (if (= (get-data 1 (modes 0)) (get-data 2 (modes 1))) 1 0))
                       :pos (+ pos 4))
        ; 'equals
        9 (-> state (update :rel-base #(+ % (get-data 1 (modes 0)))) (update :pos #(+ 2 %)))
        99 (assoc state :continue false)))))


(defn run-program [data pos rel-base output]
  (let [cmd-output (run-cmd (machine-state. true data nil '() pos rel-base))
        new-output (if (:output cmd-output)
                     (conj output (:output cmd-output))
                     output)]
    (if (:continue cmd-output)
      (recur (:data cmd-output) (:pos cmd-output) (:rel-base cmd-output) new-output)
      output)))



(defn to-map [v] (into {} (map-indexed vector v)))

(def input-data
  (to-map
    (mapv read-string (str/split (str/trim-newline (slurp "data/input13.txt")) #","))))

(def program-output (run-program input-data 0 0 []))

(def screen
  (reduce (fn [out [x y tile]] (assoc out [x y] tile)) {} (partition 3 program-output)))

(println (count (filter #(= (% 1) 2) screen)))