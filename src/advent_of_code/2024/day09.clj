(ns advent-of-code.day09
  (:require [clojure.core.reducers :as r]))

(def sample "2333133121414131402")

(def input (slurp "inputs/day09.txt"))

(defn id-files [files]
  (map-indexed vector files))

(defn expand-files [files]
  (reduce (fn [result [id blocks]]
            (into result (repeat blocks id)))
          []
          (id-files files)))

(defn parse [input]
  (let [digits (map parse-long (re-seq #"\d" input))
        files (take-nth 2 digits)]
    {:file-vec (expand-files files)
     :files files
     :free-space (take-nth 2 (drop 1 digits))}))

(defn compact-files [{:keys [file-vec files free-space]}]
  (loop [compacted-files []
         read-from :files
         file-vec file-vec
         files files
         free-space free-space]
      (if (empty? file-vec)
        compacted-files
        (case read-from
          :files
          (let [read-count (first files)]
            (recur (into compacted-files (take read-count file-vec))
                   :free-space
                   (drop read-count file-vec)
                   (drop 1 files)
                   free-space))

          :free-space
          (let [read-count (first free-space)]
            (recur (into compacted-files (reverse (take-last read-count file-vec)))
                   :files
                   (drop-last read-count file-vec)
                   files
                   (drop 1 free-space)))))))

(defn calc-checksum [compacted-files]
  (reduce + (map-indexed * compacted-files)))

(defn solve-part-1 [input]
  (let [parsed-input (parse input)
        compacted-files (compact-files parsed-input)]
    (calc-checksum compacted-files)))

;; ----------------------------------------
;;   Part 2
;; ----------------------------------------

(defn parse-2 [input]
  (let [digits (map parse-long (re-seq #"\d" input))
        files-raw (take-nth 2 digits)
        free-space (map #(assoc {} :size %) (take-nth 2 (drop 1 digits)))
        files (map-indexed (fn [idx size] {:id idx :size size}) files-raw)]
    {:fs (into [] (filter #(not= 0 (:size %))
                          (cons (first files) (interleave free-space (rest files)))))
     :check-files (into [] (reverse (range (inc (:id (last files))))))}))

(defn file? [fs-entry]
  (contains? fs-entry :id))

(defn free-space? [fs-entry]
  (not (file? fs-entry)))

(defn first-indexed [pred coll]
  (->> coll
       (map-indexed vector)
       (some (fn [[idx item]] (and (pred item) [idx item])))))

(defn fs-pull-file [file-id fs]
  (let [[file-idx file] (first-indexed #(= file-id (:id %)) fs)]
    {:file file
     :split-fs (vector (subvec fs 0 file-idx)
                       (subvec fs (inc file-idx)))}))

(defn fs-remove [idx fs]
  (vector (subvec fs 0 idx)
          (subvec fs (inc idx))))

(defn fs-ins [file fs]
  (if-let [[target-idx target-space] (first-indexed #(and (free-space? %) (>= (:size %) (:size file))) fs)]
    (let [[pre post] (fs-remove target-idx fs)]
      (into [] (concat pre
                       (filter #(> (:size %) 0) [file {:size (- (:size target-space) (:size file))}])
                       post
                       [(dissoc file :id)])))
    (conj fs file)))

(defn stringify-fs [fs]
  (apply str (map #(first (str %))
                  (expand-fs fs))))

(defn expand-fs [fs]
  (reduce (fn [fs-chars fs-entry]
            (into fs-chars (repeat (:size fs-entry) (get fs-entry :id \.))))
            []
            fs))

(defn solve-part-2 [input]
  (let [{:keys [fs check-files]} (parse-2 input)
        compacted-fs (reduce (fn [fs file-id]
                               (let [{:keys [file split-fs]} (fs-pull-file file-id fs)
                                     [search-space remaining] split-fs]
                                 (into (fs-ins file search-space) remaining)))
                             fs
                             check-files)]
    (->> compacted-fs
         expand-fs
         (map-indexed #(vector %2 %1))
         (filter #(number? (first %)))
         (map #(apply * %))
         (reduce +))))
