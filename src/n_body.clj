(ns n-body
  (:require [clojure.string :as str])
  (:use com.rpl.specter))

(defrecord body [pos vel])
(defn init-body [pos]
  (->body pos [0 0 0]))

(def moons
  (into [] (map init-body
                [[14 2 8]
                 [7 4 10]
                 [1 17 16]
                 [-4 -1 1]])))

(def test-input1
  (into [] (map init-body
                [[-1 0 2]
                 [2 -10 -7]
                 [4 -8 8]
                 [3 5 -1]])))

(def test-input2
  (into [] (map init-body
                [[-8 -10 0]
                 [5 5 10]
                 [2 -7 3]
                 [9 -8 -3]])))


(defn update-velocities [moons]
  (let [pos (select [ALL :pos] moons)
        individual-deltas (partition 4 (for [p1 pos p2 pos]
                                         (mapv compare p2 p1)))
        velocity-delta (mapv (fn [deltas] (reduce #(mapv + %1 %2) deltas)) individual-deltas)
        new-velocity (mapv (fn [v1 v2] (mapv + v1 v2)) (select [ALL :vel] moons) velocity-delta)]
    (reduce (fn [coll [ind new-val]] (setval [ind :vel] new-val coll)) moons (map-indexed vector new-velocity))))

(defn update-positions [moons]
  (defn update-position [moon]
    (assoc moon :pos (mapv + (:pos moon) (:vel moon))))
  (transform [ALL] update-position moons))

(defn energy [moon]
  (* (reduce + (map #(Math/abs %) (:pos moon)))
     (reduce + (map #(Math/abs %) (:vel moon)))))

(def simulate-step (comp update-positions update-velocities))

(defn simulate [n moons]
  (reduce (fn [moons _] (simulate-step moons)) moons (range n)))

(defn total-energy [moons]
  (reduce + (map energy moons)))

