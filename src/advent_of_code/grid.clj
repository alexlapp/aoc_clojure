(ns advent-of-code.grid)

(defn rows->grid
  "Takes a collection of rows of uniform size and returns a grid representation. Returns a grid."
  ([row-parser rows]
   (let [parsed-rows (map row-parser rows)
         max-x (- (count (first parsed-rows)) 1)
         max-y (- (count parsed-rows) 1)]
     {:grid (into [] (apply concat parsed-rows))
      :bounds [max-x max-y]}))

  ([rows]
   (rows->grid identity rows)))

(defn coll->grid
  "Takes a collection along with a method to split the collection into rows, and an optional method to parse each row. Returns a grid."
  ([row-splitter row-parser coll]
   (rows->grid row-parser (row-splitter coll)))

  ([row-splitter coll]
   (rows->grid (row-splitter coll))))

(defn in-bounds?
  [grid [x y]]
  (let [{[x-bound y-bound] :bounds} grid]
    (and (>= x 0)
         (>= y 0)
         (<= x x-bound)
         (<= y y-bound))))

(defn xy->grid-loc
  "Takes a grid and an x/y coordinate and returns the normalized grid location"
  [grid [x y]]
  (let [{[x-bound y-bound] :bounds} grid]
    (+ (* y (+ 1 x-bound)) x)))

(defn- grid-loc->xy
  "Takes a grid and a normalized location and returns an x/y coordinate"
  [grid loc]
  (let [{[x-bound y-bound] :bounds} grid
        grid-width (+ 1 x-bound)]
    [(rem loc grid-width) (quot loc grid-width)]))

(defn- in-bounds-normalized?
  [grid loc]
  (let [max-pos (xy->grid-loc grid (:bounds grid))]
    (and (>= loc 0)
         (<= loc max-pos))))

(defn get-cell
  "Takes an [x y] pair and returns the item at that cell in the grid"
  [grid pos]
  (let [target (xy->grid-loc grid pos)]
    (if (in-bounds-normalized? grid target)
      {:val (nth (:grid grid) target)
       :pos (vec pos)}
      nil)))

(defn add-points
  [p1 p2]
  (map + p1 p2))

(defn sub-points
  [p1 p2]
  (map - p1 p2))

(defn qualify-ns
  [sym]
  (symbol (name (ns-name *ns*)) (name sym)))
(defmacro def-directions
  "bindings should be sets of direction names with coordinates that find the cell in a given direction when passed to add-points"
  [& bindings]
  `(do ~@(reduce (fn [acc [dir transform]]
                   (let [pos 'pos
                         grid 'grid]
                     (-> acc
                         (conj `(defn ~(symbol (str "cell-" dir))
                                  [~grid ~pos]
                                  (get-cell ~grid (~(qualify-ns (str "pos-" dir)) ~pos))))
                         (conj `(defn ~(symbol (str "pos-" dir))
                                  [~pos]
                                  (add-points ~pos ~(qualify-ns dir))))
                         (conj `(def ~dir ~transform)))))
                 (list)
                 (partition 2 bindings))))

(def-directions
  up [0 -1]
  down [0 1]
  left [-1 0]
  right [1 0])

(defn poses-cardinal [pos]
  [(pos-up pos)
   (pos-down pos)
   (pos-left pos)
   (pos-right pos)])

(defn cells-cardinal [grid pos]
  (into [] (for [p (poses-cardinal pos)
                 :when (in-bounds? grid p)]
             (get-cell grid p))))

(defn find-cells [grid pred]
  (let [{cells :grid} grid
        indexed-cells (map-indexed vector cells)]
    (reduce (fn [matches [loc cell]]
              (if (pred cell)
                (conj matches {:val cell :pos (grid-loc->xy grid loc)})
                matches))
     []
     indexed-cells)))

(defn first-cell-val [grid pred]
  (let [{cells :grid} grid
        indexed-cells (map-indexed vector cells)]
    (some (fn [[loc cell]]
            (and (pred cell)
                 {:val cell :pos (grid-loc->xy grid loc)}))
          indexed-cells)))

(defn first-cell [grid pred]
  (let [{cells :grid} grid
        indexed-cells (map-indexed vector cells)]
    (some (fn [[loc val]]
            (let [cell {:val val :pos (grid-loc->xy grid loc)}]
              (and (pred cell) cell)))
          indexed-cells)))
