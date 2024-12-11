(ns advent-of-code.day01)

(def sample
  "3   4
4   3
2   5
1   3
3   9
3   3")

(def sample-lines (clojure.string/split-lines sample))

(def input-lines (clojure.string/split-lines (slurp "inputs/day01.txt")))

(defn parse
  [lines]
  (let [colls (reduce (fn [acc line] 
              (let [nums (map #(Integer. %) (clojure.string/split line #"\s+"))]
                [(conj (first acc) (first nums)) (conj (last acc) (last nums))]))
            [[] []]
            lines)]
    colls))

(defn parse-sample []
  (-> sample-lines
      parse))

(defn calc-part-one
  [[list-1 list-2]]
  (reduce (fn [acc pair] (+ acc (abs (apply - pair))))
   0
   (partition 2 (interleave
                 (sort list-1)
                 (sort list-2)))))

(defn calc-part-two
  [[left-list right-list]]
  (reduce (fn [acc item] (+ acc (* item (count (filter #(= item %) right-list)))))
          0
          left-list))

(defn solve-part-one
  []
  (-> input-lines
      parse
      calc-part-one))

(defn solve-part-two
  []
  (-> input-lines
      parse
      calc-part-two))

