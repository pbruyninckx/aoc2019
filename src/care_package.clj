(ns care-package
  (:require [clojure.string :as str])
  (:use com.rpl.specter))

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

(defn direction-to-code [c]
  (case c
    "a" -1
    "d" 1
    0))

(declare draw-screen)
(declare create-screen)

(defn get-direction [output]
  (let [screen (create-screen output)
        dash (select-first [ALL 0 0
                            ] (filter #(= 3 (% 1)) screen))
        ball (select-first [ALL 0 0
                            ] (filter #(= 4 (% 1)) screen))]
    (compare  ball dash)))

(defn run-program [data pos rel-base output ]
  (let [cmd-output (run-cmd (machine-state. true data nil (repeatedly #(get-direction output)) pos rel-base))
        new-output (if (:output cmd-output)
                     (conj output (:output cmd-output))
                     output)
        ;_ (if (:output cmd-output)
        ;    (draw-screen (create-screen new-output)))
        ]
    (if (:continue cmd-output)
      (recur (:data cmd-output) (:pos cmd-output) (:rel-base cmd-output) new-output)
      output)))



(defn to-map [v] (into {} (map-indexed vector v)))

(defn code-to-symbol [code]
  (case code 0 \  1 \| 2 \x 3 \- 4 \o code))

(def input-data
  (to-map
    (mapv read-string (str/split (str/trim-newline (slurp "data/input13.txt")) #","))))


(defn create-screen [raw-output]
  (reduce (fn [out [x y tile]] (assoc out [x y] tile)) {} (partition 3 raw-output)))



(def screen-size [37 20])

(defn draw-screen [screen]
  (let [empty-screen (vec (repeat (inc (screen-size 1)) (vec (repeat (inc (screen-size 0)) \0))))
        transformed-display (transform [ALL 1] code-to-symbol screen)
        display-screen (reduce (fn [screen [pos c]] (if (>= (pos 0) 0)
                                                      (assoc-in screen (reverse pos) c)
                                                      screen))
                               empty-screen
                               transformed-display)
        str-screen (mapv str/join display-screen)]
    (println (str/join "\n" (map str/join str-screen)))
    (when-first [score (filter (fn [[[x _] _]] (= x -1)) screen)]
      (println score))))


(def program-output
  (run-program input-data 0 0 []))

(def screen
  (create-screen program-output))

(println (count (filter #(= (% 1) 2) screen)))

; And perform part 2
(def play-data
  (assoc input-data 0 2))

(def play-output
  (run-program play-data 0 0 []))

(draw-screen (create-screen play-output))