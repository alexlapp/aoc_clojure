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
  (let [number-sets (->> sample
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
  (loop [to-visit pages
         seen-pages #{}]
    (let [curr-page (first to-visit)
          remaining-pages (rest to-visit)
          valid-page (empty? (set/intersection (ruleset curr-page) seen-pages))]
      (cond (not valid-page)
            false
            
            (empty? remaining-pages)
            true
            
            :else
            (recur remaining-pages (conj seen-pages curr-page))))))

(defn middle
  "Gets middle element from a sequence"
  [s]
  (nth s (quot (count s) 2)))

;; Does not work for input
(defn solve-part-1
  [input]
  (let [[ruleset-str instr-str] (split-blocks input)
        ruleset (build-rules ruleset-str)
        instrs (->> instr-str str/split-lines (map parse-longs))]
    (->> instrs
         (filter (partial is-valid-ordering? ruleset))
         (map middle)
         (reduce +))))

