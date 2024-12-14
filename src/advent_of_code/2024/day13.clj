(ns advent-of-code.2024.day13)

(def sample "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(def input (slurp "inputs/2024/day13.txt"))

(defn move-claw [p1 p2]
  (mapv + p1 p2))

(defn parse [input]
  (->> input
       (re-seq #"\d+")
       (map parse-long)
       (partition 6)
       (reduce (fn [res [ax ay bx by px py]]
                 (conj res {:prize [px py]
                            :a-btn [ax ay]
                            :b-btn [bx by]}))
               [])))

(defn valid-combos [{:keys [prize a-btn b-btn]}]
  (let [[prize-x prize-y] prize
        [a-x a-y] a-btn
        [b-x b-y] b-btn]
    (for [a-n (range 101)
          b-n (range 101)
          :when (and (= prize-x
                        (+ (* a-n a-x)
                           (* b-n b-x)))
                     (= prize-y
                        (+ (* a-n a-y)
                           (* b-n b-y))))]
      [a-n b-n])))

(defn calc-price [[a b]]
  (+ b (* 3 a)))

(defn part1 [input]
  (->> input
       parse
       (map valid-combos)
       (filter not-empty)
       (reduce (fn [total combos]
                 (+ total
                    (->> combos
                         (map calc-price)
                         sort
                         reverse
                         first)))
               0)))

(defn math-it-up [{:keys [prize a-btn b-btn]}]
  (let [[prize-x prize-y] prize
        [a-x a-y] a-btn
        [b-x b-y] b-btn]
    (let [b-top (- prize-y
                   (* (/ prize-x a-x)
                      a-y))
          b-bot (- b-y (* (/ b-x a-x)
                          a-y))
          b (/ b-top b-bot)
          a-lft (/ prize-x a-x)
          a-rht (* (/ b-x a-x)
                   b)
          a (- a-lft a-rht)]
      (if (and (integer? a)
               (integer? b))
        [a b]
        nil))))

(defn part2 [input]
  (->> input
       parse
       (map #(update % :prize (partial mapv (partial + 10000000000000))))
       (map math-it-up)
       (filter some?)
       (map calc-price)
       (reduce +)))
