(ns advent-of-code.util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def template
  "(ns advent-of-code.<YEAR>.day<DAY>)

(def sample \"\")

(def input (slurp \"inputs/<YEAR>/day<DAY>.txt\"))
")

(defn- now-year []
  (java.time.LocalDate/.getYear (java.time.LocalDate/now)))

(defn- pad-day
  [day]
  (let [day (str day)]
    (if (= 1 (count day))
      (str "0" day)
      day)))


(defn scaffold
  ([year day]
   (let [year (str year)
         day (pad-day day)
         filepath (str "./src/advent_of_code/" year "/day" day ".clj")
         contents (-> template
                      (str/replace "<YEAR>" year)
                      (str/replace "<DAY>" day))]
     (io/make-parents filepath)
     (spit filepath contents)))

  ([day]
   (scaffold (str (now-year)) day)))

