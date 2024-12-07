(ns advent-of-code.day07
  (:require [clojure.string :as str]))

(def sample "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(def input (slurp "inputs/day07.txt"))

(defn pad-zero [s n]
  (str (reduce str (repeat (- n (count s)) "0")) s))

(defn binary-strings [n]
  (map #(pad-zero % n)
       (take-while #(<= (count %) n)
                   (map #(Long/toString % 2)
                        (iterate #(inc %) 0)))))

(defn generate-operator-options [n]
  (let [operators [+ *]]
    (mapv (fn [binary] (->> binary
                           (map #(Character/digit % 10))
                           (mapv #(nth operators %))))
          (binary-strings n))))

(defn parse [input]
  (let [parse-line (fn [line] (->> line
                                   (re-seq #"\d+")
                                   (map parse-long)
                                   (apply (fn [x & xs] {:sum x :factors (into [] xs)}))))]
    (->> input
         (str/split-lines)
         (map parse-line))))

