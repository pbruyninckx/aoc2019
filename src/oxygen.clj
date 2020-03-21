(ns oxygen
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
              rest-modes (quot modes 10)]
          (recur rest-modes (conj ret mode))))))
  (instruction. (rem instruction-code 100) (split-modes (quot instruction-code 100))))

(defprotocol IntComputerHandler
  (provide-input [ich])
  (handle-output [ich out]))

(defrecord machine-state [continue data ich pos rel-base])

(defn run-cmd [state]
  (let [{:keys [data ich pos rel-base]} state]
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
        3 (let [[new-input new-ich] (provide-input ich)]
            (assoc state :data

                         (save-data 1 (modes 0)
                                    new-input)
                         :pos (+ pos 2)
                         :ich new-ich))
        4 (assoc state :ich (handle-output ich (get-data 1 ((:modes instruction) 0)))
                       :pos (+ pos 2))
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

(defn create-walker []
  (let [walker {:data []
                :input (fn [w] (update w :data conj (Integer/parseInt (read-line)) ))}]
        walker))


(def num->directions {1 :up 2 :down 3 :left 4 :right})
(defn reverse-direction [d]
  ({:up :down :down :up :left :right :right :left} d))
(def directions->num (into {} (map (comp vec reverse) num->directions)))
(def directions (mapv second num->directions))
(def direction-map
  (into {} (map vector directions (rest directions))))

(defn direction-after [d]
  (direction-map d))

(defn move-position [position direction]
  (case direction
    :up (update position 0 inc)
    :down (update position 0 dec)
    :left (update position 1 dec)
    :right (update position 1 inc)))

(defrecord DroidMover [position seen path past-directions next-direction finished]
  IntComputerHandler
  (provide-input [ich]
    (cond (:finished ich)
          [-1 ich]
          next-direction
          ; try to move
          (let [predicted-position (move-position position next-direction)]
            (if (contains? seen predicted-position)
              (provide-input (update ich :next-direction direction-after)) ;TODO Why is recur not working?
              [(directions->num next-direction)
               (-> ich
                   (assoc :next-direction (first directions))
                   (update :past-directions conj next-direction))]))
          ; backtrack
          :else
          (let [last-direction (peek past-directions)
                backtrack-direction (reverse-direction last-direction)]
            [(directions->num backtrack-direction)
             (-> ich
                 (update :past-directions conj backtrack-direction)
                 (assoc :next-direction last-direction))]))) ; can be optimised - necessary?
  (handle-output [ich out]
    (let [new-position (move-position position (peek past-directions))]
      (case out
        0                                                   ;don't move
        (-> ich
            (update :past-directions pop)
            (update :next-direction direction-after)
            (update :seen conj new-position))
        1 ;move in desired direction
          (if (and (>= (count path) 2) (= new-position (-> path pop peek)))
            (-> ich ;backtrack
              (assoc :position new-position)
              (update :path pop)
                (update :past-directions (comp pop pop))
              (assoc :next-direction (-> past-directions pop peek reverse-direction)))
          (-> ich ; new
            (update :seen conj new-position)
            (assoc :position new-position)
            (update :path conj new-position)
            (assoc :next-direction (first directions))))
        2                                                  ;finished
        (do
          (println "'t Is gedaan: " (count (:path ich)))
          (assoc ich :finished true))))))

(defn run-program [data pos rel-base ich]
  (let [cmd-output (run-cmd (machine-state. true data ich pos rel-base))
        new-ich (:ich cmd-output)]
    (if (:continue cmd-output)
      (recur (:data cmd-output) (:pos cmd-output) (:rel-base cmd-output) new-ich)
      nil)))

(defn to-map [v] (into {} (map-indexed vector v)))

(def input-data
  (to-map
    (mapv read-string (str/split (str/trim-newline (slurp "data/input15.txt")) #","))))



(defn create-new-dm []
  (let [start [0 0]]
   (->DroidMover start #{start} [start] [] (first directions) false)))

;(run-program input-data 0 0 (create-new-dm))
(defn test-program []
  (run-program input-data 0 0 (create-new-dm)))
