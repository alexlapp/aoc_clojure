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
         visited #{}
         peaks #{}]
    (let [[visiting & to-visit] to-visit]
      (cond (nil? visiting)
            (count peaks)

            (contains? visited visiting)
            (recur to-visit visited peaks)

            :else
            (let [cardinal-cells (grid/cells-cardinal grid (:pos visiting))]
              (recur (into to-visit (filter #(= (:val %) (+ 1 (:val visiting))) cardinal-cells))
                     (conj visited visiting)
                     (cond-> peaks
                       (= (:val visiting) 9) (conj visiting))))))))

(defn solve-part-1 [input]
  (let [grid (parse input)
        starting-points (grid/find-cells grid #{0})]
    (->> starting-points
         (map #(score-trailhead grid %))
         (reduce +))))

;; ----------------------------------------
;;   Part 2
;; ----------------------------------------

(defn rate-trailhead [grid start-cell]
  (loop [to-visit [start-cell]
         peaks []]
    (let [[visiting & to-visit] to-visit]
      (cond (nil? visiting)
            (count peaks)

            :else
            (let [cardinal-cells (grid/cells-cardinal grid (:pos visiting))]
              (recur (into to-visit (filter #(= (:val %) (+ 1 (:val visiting))) cardinal-cells))
                     (cond-> peaks
                       (= (:val visiting) 9) (conj visiting))))))))

(defn solve-part-2 [input]
  (let [grid (parse input)
        starting-points (grid/find-cells grid #{0})]
    (->> starting-points
         (map #(rate-trailhead grid %))
         (reduce +))))
