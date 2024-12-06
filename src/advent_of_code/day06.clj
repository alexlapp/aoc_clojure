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

(defn indexes [pred coll]
  (keep-indexed (fn [idx item]
                  (if (pred item) idx))
                coll))

(defn parse [input]
  (let [builder (fn [state [idx line]]
                  (let [obstacles (->> line
                                       (indexes #(= \# %))
                                       (map #(vector % idx)))
                        guard (->> line
                                   (indexes #(= \^ %))
                                   first)]
                    (if (nil? guard)
                      (update-in state [:obstacles] #(apply conj % obstacles))
                    (-> state
                        (update-in [:obstacles] #(apply conj % obstacles))
                        (assoc-in [:start] [guard idx])))))]
    (let [indexed-lines (->> input
                             str/split-lines
                             (map-indexed vector))
          last-line (last indexed-lines)]
      (reduce builder {:obstacles #{}
                       :start nil
                       :dir [0 -1]
                       :bounds ((fn [[idx string]] (vector idx (- (count string) 1))) last-line)}
              indexed-lines))))


(defn out-of-bounds? [[xbound ybound] [x y]]
  (or (< x 0)
      (< y 0)
      (> x xbound)
      (> y ybound)))

(defn contains-obstacle? [obstacles pos]
  (contains? obstacles pos))

(defn turn-right [dir]
  (cond (= [0 -1] dir) [1 0]
        (= [1 0] dir) [0 1]
        (= [0 1] dir) [-1 0]
        (= [-1 0] dir) [0 -1]))

(defn add-vec [v1 v2]
  (mapv + v1 v2))

(defn walk [{:keys [obstacles start dir bounds]}]
  (let [contains-obstacle? (partial contains-obstacle? obstacles)
        out-of-bounds? (partial out-of-bounds? bounds)]
    (loop [traversed (transient #{start})
           collisions #{}
           pos start
           dir dir]
      (let [next-pos (add-vec pos dir)]
        (cond (out-of-bounds? next-pos)
              (persistent! traversed)

              (contains-obstacle? next-pos)
              (let [obs-dir-pair [pos dir]]
                (if (contains? collisions obs-dir-pair)
                  :loop
                  (recur traversed (conj collisions obs-dir-pair) pos (turn-right dir))))
              

              :else
              (recur (conj! traversed next-pos) collisions next-pos dir))))))

(defn solve-part-1 [input]
  (-> input
      parse
      walk
      count))

;; Runtime: ~20 seconds for test input
(defn solve-part-2 [input]
  (let [state (-> input
                  parse)
        guard-path (walk state)
        candidates (disj guard-path (state :start))]
    (count (reduce (fn [acc candidate]
                     (if (= :loop (walk (update-in state [:obstacles] #(conj % candidate))))
                       (conj acc candidate)
                       acc))
                   []
                   candidates))))

(comment
  (def sample-parse-output
    {:obstacles #{[8 7] [2 3] [7 4] [9 1] [6 9] [1 6] [0 8] [4 0]}
     :start [4 6]
     :dir [0 -1]
     :bounds [9 9]}))
