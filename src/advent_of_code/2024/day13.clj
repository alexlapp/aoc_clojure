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

(def initial-search-state
  {:total-cost 0
   :pos [0 0]
   :a-btn 0
   :b-btn 0})

(defn push-a [machine search-state]
  (-> search-state
      (update :a-btn inc)
      (update :total-cost #(+ 3 %))
      (update :pos #(move-claw % (:a-btn machine)))))

(defn push-b [machine search-state]
  (-> search-state
      (update :b-btn inc)
      (update :total-cost inc)
      (update :pos #(move-claw % (:b-btn machine)))))

(defn is-past-prize? [{:keys [prize]} {:keys [pos]}]
  (or (< (first prize) (first pos))
      (< (last prize) (last pos))))

(defn is-max-presses? [{:keys [a-btn b-btn]}]
  (or (> b-btn 100)
      (> a-btn 100)))

(defn over-prize? [{:keys [prize]} {:keys [pos]}]
  (= prize pos))

(defn expand [machine search-state]
  (map (fn [xform] (xform machine search-state))
       [push-a push-b]))

(defn search [machine]
  (let [xform]
    (loop [search-state [initial-search-state]
           seen #{}
           winning {:total-cost (Long/MAX_VALUE)}]
      (->> search-state
           (mapcat expand)
           (filter (fn [{:keys [pos a-btn b-btn]}] (contains? seen [pos a-btn b-btn])))
           ))))
