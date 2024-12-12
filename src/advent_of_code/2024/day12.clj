(ns advent-of-code.2024.day12
  (:require [advent-of-code.grid :as grid]
            [clojure.string :as str]
            [clojure.set :as set]))

(def simple (str "AAAA" "\n"
                 "BBCD" "\n"
                 "BBCC" "\n"
                 "EEEC"))

(def sample (str "RRRRIICCFF" "\n"
                 "RRRRIICCCF" "\n"
                 "VVRRRCCFFF" "\n"
                 "VVRCCCJFFF" "\n"
                 "VVVVCJJCFE" "\n"
                 "VVIVCCJJEE" "\n"
                 "VVIIICJJEE" "\n"
                 "MIIIIIJJEE" "\n"
                 "MIIISIJEEE" "\n"
                 "MMMISSJEEE"))

(def input (slurp "inputs/2024/day12.txt"))

(defn parse [input]
  (grid/coll->grid str/split-lines input))

(defn build-region [grid start-cell]
  (loop [[visiting & to-visit] [start-cell]
         region (transient #{})]
    (if (nil? visiting)
      (persistent! region)
      
      (let [region (conj! region (:pos visiting))
            cardinal (grid/cells-cardinal grid (:pos visiting))
            candidates (filter #(and (not (contains? region (:pos %)))
                                     (= (:val visiting) (:val %)))
                               cardinal)]
        (recur (into to-visit candidates) region)))))

(defn build-all-regions [grid]
  (loop [mapped #{}
         regions (transient [])]
    (if-let [start-cell (grid/first-cell grid #(not (contains? mapped (:pos %))))]
      (let [region (build-region grid start-cell)]
        (recur (set/union mapped region) (conj! regions region)))
      (persistent! regions))))

(defn region-price [region]
  (* (count region)
     (reduce (fn [sum pos]
               (let [candidates (grid/poses-cardinal pos)]
                 (+ sum
                    (count (filter #(not (contains? region %)) candidates)))))
             0
             region)))

(defn solve-part-1 [input]
  (let [grid (parse input)
        regions (build-all-regions grid)]
    (reduce + (map region-price regions))))

;; ----------------------------------------
;;   Part 2
;; ----------------------------------------

(defn split-rows [region]
  (->> region
       (reduce (fn [rows pos]
                 (update rows (last pos) (fnil #(conj % pos) [])))
               {})
       (into {} (map (fn [[row-y cells]]
                       (let [sorted-cells (sort-by first cells)]
                         [row-y (reduce (fn [cell-blocks cell]
                                          (let [prev-cell (last (last cell-blocks))]
                                            (if (> (- (first cell) (first prev-cell)) 1)
                                              (conj cell-blocks [cell])
                                              (update cell-blocks (- (count cell-blocks) 1) #(conj % cell)))))
                                        [[(first sorted-cells)]]
                                        (drop 1 sorted-cells))]))))))

(defn split-cols [region]
  (->> region
       (reduce (fn [rows pos]
                 (update rows (first pos) (fnil #(conj % pos) [])))
               {})
       (into {} (map (fn [[col-x cells]]
                       (let [sorted-cells (sort-by last cells)]
                         [col-x (reduce (fn [cell-blocks cell]
                                          (let [prev-cell (last (last cell-blocks))]
                                            (if (> (- (last cell) (last prev-cell)) 1)
                                              (conj cell-blocks [cell])
                                              (update cell-blocks (- (count cell-blocks) 1) #(conj % cell)))))
                                        [[(first sorted-cells)]]
                                        (drop 1 sorted-cells))]))))))

(defn get-side-count [check-set xform positions]
  (->> positions
       (map (fn [pos] (if (contains? check-set (xform pos))
                        :covered
                        :exposed)))
       (partition-by #{:exposed})
       (filter #(= :exposed (first %)))
       count))

(defn count-horizontal-sides [region]
  (let [row-map (split-rows region)]
    (reduce (fn [total [row-y cell-blocks]]
              (let [above (into #{} (partitionv 2 (flatten (get row-map (dec row-y)))))
                    below (into #{} (partitionv 2 (flatten (get row-map (inc row-y)))))
                    above-side-count (partial get-side-count above grid/pos-up)
                    below-side-count (partial get-side-count below grid/pos-down)]
                (+ total
                   (reduce (fn [running-total block]
                             (+ running-total
                                (above-side-count block)
                                (below-side-count block)))
                           0
                           cell-blocks))))
            0
            row-map)))

(defn count-vertical-sides [region]
  (let [col-map (split-cols region)]
    (reduce (fn [total [col-x cell-blocks]]
              (let [left (into #{} (partitionv 2 (flatten (get col-map (dec col-x)))))
                    right (into #{} (partitionv 2 (flatten (get col-map (inc col-x)))))
                    left-side-count (partial get-side-count left grid/pos-left)
                    right-side-count (partial get-side-count right grid/pos-right)]
                (+ total
                   (reduce (fn [running-total block]
                             (+ running-total
                                (left-side-count block)
                                (right-side-count block)))
                           0
                           cell-blocks))))
            0
            col-map)))

(defn count-sides [region]
  (+ (count-horizontal-sides region)
     (count-vertical-sides region)))

(defn region-price-siding [region]
  (* (count region)
     (count-sides region)))

(defn solve-part-2 [input]
  (let [grid (parse input)
        regions (build-all-regions grid)]
    (reduce + (map region-price-siding regions))))
