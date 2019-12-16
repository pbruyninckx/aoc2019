(ns secure)

(defn get-digits [n] (map #(Character/digit % 10) (str n)))

(defn valid-password [digits]
  (and (every? identity (map <= digits (rest digits)))
       (some #(= % 2) (map second (frequencies digits)))))

(def numbers-as-digits (map get-digits (range 183564 657474)))

(print (count (filter valid-password numbers-as-digits)))