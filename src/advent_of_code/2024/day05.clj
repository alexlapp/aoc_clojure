(ns advent-of-code.day05
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(def input (slurp "inputs/day05.txt"))

(defn split-blocks
  [input]
  (str/split input #"\n\n|\r\n\r\n"))

(defn parse-longs
  [string]
  (->> string
       (re-seq #"\d+")
       (map parse-long)))

(defn build-rules
  [rule-str]
  (let [number-sets (->> rule-str
                         split-blocks
                         first
                         parse-longs
                         (partition 2))]
    (reduce (fn [ruleset [before after]]
              (update-in ruleset [before] #(set (cons after %))))
            {}
            number-sets)))

(defn is-valid-ordering?
  [ruleset pages]
  (loop [[curr-page & remaining-pages] pages
         seen-pages #{}]
    (cond (some #((get ruleset curr-page #{}) %) seen-pages)
          false
          
          (empty? remaining-pages)
          true
          
          :else
          (recur remaining-pages (conj seen-pages curr-page)))))

(defn middle
  "Gets middle element from a sequence"
  [s]
  (nth s (quot (count s) 2)))

(defn solve-part-1
  [input]
  (let [[ruleset-str instr-str] (split-blocks input)
        ruleset (build-rules ruleset-str)
        instrs (->> instr-str str/split-lines (map parse-longs))]
    (->> instrs
         (filter (partial is-valid-ordering? ruleset))
         (map middle)
         (reduce +))))

;; ----------------------------------------
;;   Part 2
;; ----------------------------------------

(defn insert-at [n xs x]
  (into []
        (concat (take n xs)
                [x]
                (drop n xs))))

(defn reorder [rules instruction]
  (let [add-page
        (fn [pages page]
          (let [deps (get rules page #{})
                invalid-pos (first
                             (for [n (range (count pages))
                                   :when (contains? deps (nth pages n))]
                               n))]
            (if invalid-pos
              (insert-at invalid-pos pages page)
              (conj pages page))))]
    (reduce add-page [] instruction)))

(defn solve-part-2
  [input]
  (let [[ruleset-str instructions-str] (split-blocks input)
        ruleset (build-rules ruleset-str)
        instructions (->> instructions-str str/split-lines (map parse-longs))]
    (->> instructions
         (filter #(not (is-valid-ordering? ruleset %)))
         (map (partial reorder ruleset))
         (map middle)
         (reduce +))))
