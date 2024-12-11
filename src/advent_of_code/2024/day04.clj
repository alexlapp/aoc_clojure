(ns advent-of-code.day04
  (:require [clojure.string :as str]))

(def sample "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(def simple-sample "....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX")

(def input (slurp "inputs/day04.txt"))

(defn shift-string-left
  "Shifts string left by n characters, wrapping characters"
  [string n]
  (let [string-length (count string)
        double-string (apply str (repeat 2 string))]
    (apply str (take string-length (drop (mod n string-length) double-string)))))

(defn shift-string-right
  "Shifts string right by n characters, wrapping characters"
  [string n]
  (let [string-length (count string)
        double-string (apply str (repeat 2 string))]
    (apply str (take string-length (drop (- string-length (mod n string-length)) double-string)))))

(defn rotate-search-space
  "Takes a list of strings representing lines, and rotates 90 degrees counter-clockwise. Result is equivalent of columns rather than rows"
  [search-space]
  (apply map str search-space))

(defn generate-diagonal-search-space
  [shift-fn search-space]
  (->> search-space
       (map-indexed (fn [idx line] (shift-fn line idx)))
       (rotate-search-space)))

(defn generate-search-spaces
  "Returns list of lists of searchable lines representing horizontal, vertical, and diagonal"
  [input]
  (let [lines (map #(str "." % ".") (str/split-lines input))
        horizontal-search lines
        vertical-search (rotate-search-space lines)
        diagonal-desc-search (generate-diagonal-search-space shift-string-left lines)
        diagonal-asc-search (generate-diagonal-search-space shift-string-right lines)]
    [horizontal-search vertical-search diagonal-desc-search diagonal-asc-search]))

(defn search-search-space
  [re search-space]
  (reduce (fn [acc line] (+ acc (count (re-seq re line))))
          0
          search-space))

(defn solve-part-1
  [input]
  (let [re #"(?=(XMAS|SAMX))"]
    (reduce (fn [sum search-space] (+ sum (search-search-space re search-space)))
            0
            (generate-search-spaces input))))

;; ----------------------------------------
;;   Part 2
;; ----------------------------------------
(def basic "M.S
.A.
M.S")

(defn positions
  [pred coll]
  (keep-indexed (fn [idx x] (when (pred x)
                              idx))
                coll))

(defn find-chars
  [c string]
  (positions (partial = c) string))

(defn check-set
  [search-set]
  (let [top-row (first search-set)
        mid-row (second search-set)
        bot-row (last search-set)
        top-ms (set (find-chars \M top-row))
        top-ss (set (find-chars \S top-row))
        mid-as (find-chars \A mid-row)
        bot-ms (set (find-chars \M bot-row))
        bot-ss (set (find-chars \S bot-row))
        valid-as (filter #(not (or (= 0 %)
                                   (= (- 1 (count mid-row)) %)))
                         mid-as)]
    (reduce (fn [xcount a-idx]
              (if (and (or (and (contains? top-ms (- a-idx 1))
                                (contains? bot-ss (+ a-idx 1)))

                           (and (contains? top-ss (- a-idx 1))
                                (contains? bot-ms (+ a-idx 1))))

                       (or (and (contains? bot-ms (- a-idx 1))
                                (contains? top-ss (+ a-idx 1)))

                           (and (contains? bot-ss (- a-idx 1))
                                (contains? top-ms (+ a-idx 1)))))
                (+ xcount 1)
                xcount))
            0
            valid-as)))

(or (and (contains? top-ms (- % 1))
         (contains? bot-ss (+ % 1)))

    (and (contains? top-ss (- % 1))
         (contains? bot-ms (+ % 1))))

(defn solve-part-2
  [input]
  (let [sets (->> input
                  str/split-lines
                  (partition 3 1))]
    (reduce + (map check-set sets))))

