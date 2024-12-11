(ns advent-of-code.day10
  (:require [clojure.string :as str]
            [advent-of-code.grid :as grid]))

(def sample (str/join
             "\n"
             ["89010123"
              "78121874"
              "87430965"
              "96549874"
              "45678903"
              "32019012"
              "01329801"
              "10456732"]))

(def input (slurp "inputs/day10.txt"))

(defn parse [input]
  (grid/coll->grid str/split-lines
                   #(map parse-long (re-seq #"\d" %))
                   input))


(defn score-trailhead [grid start-cell]
  (loop [to-visit [start-cell]
         visited #{}]
    (let [[visiting & to-visit] to-visit]
      (cond (nil? visiting)
            (count (filter #(= 9 (:val %)) visited))

            (contains? visited visiting)
            (recur to-visit)

            :else
            TODO: ADD REMAINING TO TO-VISIT))))
