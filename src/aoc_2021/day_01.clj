(ns aoc-2021.day-01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-1-str "199
200
208
210
200
207
240
269
260
263")

(defn input-str->seq
  [input-str]
  (->> input-str
       (str/split-lines)
       (mapv parse-long)))

(defn num-increasing-elevation-1
  "This relies on map's ability to create pairs from two input sequences. 
   The first sequence skips the first element. That way the pairs are 
 	 current elevation and previous elevation"
  [elevations]
  (->> (map > (rest elevations) elevations)
       (filter identity)
       (count)))

(defn num-increasing-elevation-2
  "This uses partition with a step to get adjacent rolling pairs"
  [elevations]
  (->> (partition 2 1 elevations)
       (map (fn [[prev curr]] (> curr prev)))
       (filter identity)
       count))

(defn num-increasing-elevations-group-of-3
  "This is solving Part 2 of the problem"
  [elevations]
  (->> elevations
       (partition 3 1)
       (map (partial apply +))
       (partition 2 1)
       (filter (fn [[prev curr]] (> curr prev)))
       (count)))

(def challenge-input (->> "inputs/day-01.txt"
                          (io/resource)
                          slurp
                          input-str->seq))

(assert
 (= (num-increasing-elevation-1 challenge-input)
    (num-increasing-elevation-2 challenge-input)))

(assert
 (= 1600
    (num-increasing-elevations-group-of-3 challenge-input)))

(comment
  (def elevations (input-str->seq input-1-str))

  (num-increasing-elevations-group-of-3 challenge-input))

