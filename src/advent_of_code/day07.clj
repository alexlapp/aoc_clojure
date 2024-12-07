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

(defn base-x-strings [x n]
  (map #(pad-zero % n)
       (take-while #(<= (count %) n)
                   (map #(Long/toString % x)
                        (iterate #(inc %) 0)))))

(defn generate-operator-options [operators n]
  (mapv (fn [binary] (->> binary
                          (map #(Character/digit % 10))
                          (mapv #(nth operators %))))
        (base-x-strings (count operators) n)))

(defn parse [input]
  (let [parse-line (fn [line] (->> line
                                   (re-seq #"\d+")
                                   (map parse-long)
                                   (apply (fn [x & xs] {:sum x :factors (into [] xs)}))))]
    (->> input
         (str/split-lines)
         (mapv parse-line))))

(defn combine [factors operators]
  (loop [acc (first factors)
         [factor & remaining-factors] (rest factors)
         [operator & remaining-operators] operators]
    (if (nil? factor)
      acc
      (recur (operator acc factor) remaining-factors remaining-operators))))

(defn is-entry-valid? [operators {:keys [sum factors]}]
  (let [operator-count (- (count factors) 1)
        operator-combos (generate-operator-options operators operator-count)]
    (some (fn [operators]
            (= sum (combine factors operators)))
          operator-combos)))

(defn solve-part-1 [input]
  (let [entries (parse input)]
    (reduce (fn [sum entry]
              (if (is-entry-valid? [+ *] entry)
                (+ sum (:sum entry))
                sum))
            0
            entries)))

(defn || [x y]
  (parse-long (str x y)))

(defn solve-part-2 [input]
  (let [entries (parse input)]
    (reduce (fn [sum entry]
              (if (is-entry-valid? [+ * ||] entry)
                (+ sum (:sum entry))
                sum))
            0
            entries)))

(defn a-lt-solve-part-2 [input]
  (let [entries (parse input)]
    (->> entries
         (pmap (fn [entry]
                 {:valid? (is-entry-valid? [+ * ||] entry)
                  :sum (:sum entry)}))
         (reduce (fn [sum solved-entry]
                   (if (:valid? solved-entry)
                     (+ sum (:sum solved-entry))
                     sum))
                 0))))

