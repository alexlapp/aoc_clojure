(ns advent-of-code.day08
  (:require [clojure.string :as str]))

; ............
; ........0...
; .....0......
; .......0....
; ....0.......
; ......A.....
; ............
; ............
; ........A...
; .........A..
; ............
; ............ 

(def sample "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(def input (slurp "inputs/day08.txt"))

(defn parse [input]
  (let [lines (str/split-lines input)
        indexed-lines (map-indexed vector lines)
        max-col (- (count (first lines)) 1)
        max-row (first (last indexed-lines))]
    {:bounds [max-col max-row]
     :antennas (reduce (fn [antennas [row line]]
                         (let [entries (->> line
                                            (map-indexed (comp reverse vector))
                                            (filter (fn [[c idx]] (not= c \.))))]
                           (loop [antennas antennas
                                  [[c col] & remaining-entries] entries]
                               (if (nil? c)
                                 antennas
                                 (recur (update-in antennas [c] #(conj (or % []) [col row])) remaining-entries)))))
                       {}
                       indexed-lines)}))

(defn sub-points [p1 p2]
  (mapv - p1 p2))

(defn add-points [p1 p2]
  (mapv + p1 p2))

(defn calc-antinodes [a1 a2]
  (let [dist (sub-points a1 a2)]
    [(add-points a1 dist)
     (sub-points a2 dist)]))

(defn find-antinodes [antennas]
  (loop [combos []
         [first & remaining] antennas]
    (if (empty? remaining)
      (vec combos)
      (recur (into combos (mapcat (fn [second] (calc-antinodes first second)) remaining))
             remaining))))

(defn in-bounds? [[x-bound y-bound] [x-ant y-ant]]
  (and (>= x-ant 0)
       (>= y-ant 0)
       (<= x-ant x-bound)
       (<= y-ant y-bound)))

(defn solve-part-1 [input]
  (let [{:keys [bounds antennas]} (parse input)
        in-bounds? (partial in-bounds? bounds)]
    (->> antennas
         (reduce (fn [result [_ v]] 
                   (into result (find-antinodes v)))
                 #{})
         (filter in-bounds?)
         count)))


;; ----------------------------------------
;;   Part 2
;; ----------------------------------------

(defn calc-antinodes-r [bounds-check a1 a2]
  (let [dist (sub-points a1 a2)]
    (concat (take-while bounds-check (iterate #(add-points % dist) a1))
            (take-while bounds-check (iterate #(sub-points % dist) a2)))))

(defn find-antinodes-r [bounds-check antennas]
  (loop [combos []
         [first & remaining] antennas]
    (if (empty? remaining)
      (vec combos)
      (recur (into combos (mapcat (fn [second] (calc-antinodes-r bounds-check first second)) remaining))
             remaining))))

(defn solve-part-2 [input]
  (let [{:keys [bounds antennas]} (parse input)
        in-bounds? (partial in-bounds? bounds)]
    (->> antennas
         (reduce (fn [result [_ v]]
                   (into result (find-antinodes-r in-bounds? v)))
                 #{})
         count)))

