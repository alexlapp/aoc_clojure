(ns advent-of-code.day11
  (:require [clojure.string :as str]))

(def sample "125 17")

(def input (slurp "inputs/day11.txt"))

(defn trim-leading-zeros [pred s]
  (let [out (apply str (drop-while pred s))]
    (if (empty? out)
      "0"
      out)))

(defn proc-stone [stone]
  (cond (= stone "0")
        ["1"]

        (= 0 (rem (count stone) 2))
        (let [half (/ (count stone) 2)]
          [(apply str (take half stone))
           (trim-leading-zeros #{\0} (drop half stone))])

        :else
        [(str (* 2024 (parse-long stone)))]))

(defn blink
  ([stones]
   (persistent!
    (reduce (fn [new-stones stone]
              (reduce conj! new-stones (proc-stone stone)))
            (transient [])
            stones)))

  ([n stones]
   (nth (iterate blink stones) n)))

(defn solve-part-1 [input]
  (let [initial-stones (-> input
                           str/trim
                           (str/split #" "))]
    (->> initial-stones
         (blink 25)
         count)))

(def blink!
  (memoize
   (fn [stone ntimes]
     (cond
       (= ntimes 0)
       1

       (= stone 0)
       (blink! 1 (dec ntimes))

       (even? (count (str stone)))
       (let [text (str stone)
             n (count text)
             [l r] (mapv parse-long
                         (map #(apply str %)
                              (split-at (/ n 2) text)))]
         (+ (blink! l (dec ntimes))
            (blink! r (dec ntimes))))

       :else
       (blink! (* 2024 stone) (dec ntimes))))))

(defn solve-part-2 [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       (map #(blink! % 75))
       (reduce +)))
