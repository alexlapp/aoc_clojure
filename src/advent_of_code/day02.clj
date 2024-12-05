(ns advent-of-code.day02
  (:require [clojure.string :as str]))

(def sample "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(def input (slurp "inputs/day02.txt"))

;; Part 1
(defn gaps
  [report]
  (map #(apply - (reverse %))
       (partition 2 1 report)))

(defn safe-report?
  [report]
  (let [report-gaps (gaps report)]
    (and (or (every? #(< 0 %) report-gaps)
             (every? #(> 0 %) report-gaps))
         (every? #(<= % 3) (map abs report-gaps)))))

(defn solve-part-1
  [input]
  (->> input
       str/split-lines
       (map (fn [line] (map #(Integer. %) (re-seq #"\d+" line))))
       (filter safe-report?)
       count))

;; Part 2
(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (into (subvec coll 0 pos) (subvec coll (inc pos))))

(defn one-removed-vecs
  "returns all possible combinations of a vector if one element is removed"
  [in]
  (let [v (into [] in)]
    (map #(vec-remove % v)
         (range (count v)))))

(defn dampened-safe-report?
  [report]
  (if (safe-report? report)
    true
    (some safe-report? (one-removed-vecs report))))

(defn solve-part-2
  [input]
  (->> input
       str/split-lines
       (map (fn [line] (map #(Integer. %) (re-seq #"\d+" line))))
       (filter dampened-safe-report?)
       count))

(defn alt-solve-part-2
  [input]
  (let [parsed-lines (persistent! (reduce (fn [res line]
                                            (->> line
                                                   (re-seq #"\d+")
                                                   (map #(Integer. %))
                                                   (dampened-safe-report?)
                                                   (conj! res)))
                                          (transient [])
                                          (into [] (str/split-lines input))))]
    (->> parsed-lines
         (filter #(= true %))
         count)))

