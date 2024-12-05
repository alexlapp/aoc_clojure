(ns advent-of-code.day03)

(def sample-1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def input (slurp "inputs/day03.txt"))

(defn solve-part-1
  [input]
  (->> input
       (re-seq #"mul\((\d+),(\d+)\)")
       (map #(map parse-long (drop 1 %)))
       (map #(apply * %))
       (apply +)))


(def sample-2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn solve-part-2
  [input]
  (->> input
       (re-seq #"mul\((\d+),(\d+)\)|don't\(\)|do\(\)")
       (reduce (fn [[calc sum] match]
                 (let [instr (first match)
                       n1 (parse-long (or (nth match 1) "0"))
                       n2 (parse-long (or (nth match 2) "0"))]
                   (cond (= instr "don't()") [false sum]
                         (= instr "do()") [true sum]
                         (= calc false) [calc sum]
                         :else [calc (+ sum (* n1 n2))])))
               [true 0])
       last))

