(ns advent-of-code.day06
  (:require [clojure.string :as str]))

(def sample "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
")

(def input (slurp "inputs/day06.txt"))

;; sample parsing
{:obstacles #{[1 2] [3 2]}
 :start-pos [6 4]}

(defn indexes [pred coll]
  (keep-indexed (fn [idx item]
                  (if (pred item) idx))
                coll))

(defn parse [input]
  (let [builder (fn [state [idx line]]
                  (let [obstacles (->> line
                                       (indexes #(= \# %))
                                       (map #(vector idx %)))
                        guard (->> line
                                   (indexes #(= \^ %))
                                   first)]
                    (if (nil? guard)
                      (update-in state [:obstacles] #(apply conj % obstacles))
                    (-> state
                        (update-in [:obstacles] #(apply conj % obstacles))
                        (assoc-in [:guard] [idx guard])))))]
    (->> input
         str/split-lines
         (map-indexed vector)
         (reduce builder {:obstacles [] :guard nil}))))



