(ns stoichiometry
  (:require [clojure.string :as str]))

(defrecord product-state
  [ore created rest])
(defrecord chem-quantity [chemical quantity])
(defrecord reaction
  [input output])

(def input1
  "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL")
(def input2
  "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL")

(defn parse-chem-quantity [s]
  (let [[quantity chem] (str/split s #" ")]
    (->chem-quantity chem (Integer/parseInt quantity))))

(defn parse-input-line [line]
  (let [[all-ingredients result] (str/split line #" => ")
        ingredients (str/split all-ingredients #", ")]
    (->reaction
      (mapv parse-chem-quantity ingredients)
      (parse-chem-quantity result))))

(defn parse-input [s]
  (let [rules (mapv parse-input-line (str/split-lines s))]
    (into {} (map (fn [rule] [(-> rule :output :chemical) rule]) rules))))

(defn round-up-division [a b]
  (+ (Integer/divideUnsigned a b)
     (if (= 0 (Integer/remainderUnsigned a b))
       0 1)))

(defn move-ore [state]
  (if (contains? (:created state) "ORE")
    (-> state
        (update :created dissoc "ORE")
        (update :ore + ((:created state) "ORE")))
    state))

(defn solve [rules]
  (loop [state (->product-state 0 {"FUEL" 1} {})]
    (if (empty? (:created state))
      (:ore state)
      (let [[product quantity] (first (:created state))]
        (if (contains? (:rest state) product)
          (case (compare quantity ((:rest state) product))
            -1 (recur (-> state (update-in [:created] dissoc product)
                          (update-in [:rest product] - quantity)))
            0 (recur (-> state (update-in [:created] dissoc product)
                         (update-in [:rest] dissoc product)))
            1 (recur (-> state (update-in [:created product] - (get-in state [:rest product]))
                         (update-in [:rest] dissoc product))))
          (let [{:keys [input output]} (rules product)
                repeats (round-up-division quantity (:quantity output))
                to-create (mapv #(update % :quantity * repeats) input)
                new-created (reduce (fn [coll {:keys [chemical quantity]}]
                                      (if (contains? coll chemical)
                                        (update coll chemical + quantity)
                                        (assoc coll chemical quantity)))
                                    (:created state)
                                    to-create)
                to-rest (- (* repeats (:quantity output)) quantity)
                new-rest (assoc (:rest state) product to-rest)]
            (recur (-> state (assoc :created (dissoc new-created product) :rest new-rest)
                       (move-ore)))))))))

(def input (slurp "data/input14.txt"))
(println (solve (parse-input input)))