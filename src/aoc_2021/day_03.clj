(ns aoc-2021.day-03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def sample-input-str "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(defn binary-str->vec
  [binary-str]
  (->> binary-str
       (map (comp parse-long str))
       vec))

(defn binary-vec->decimal
  [binary-number]
  (->> binary-number
       (reverse)
       (map-indexed (fn [index bit]
                      (long (* bit (Math/pow 2 index)))))
       (reduce + 0)))

(defn input-str->binary-nums
  [input-str]
  (->> input-str
       (str/split-lines)
       (mapv binary-str->vec)))

(defn digit-frequencies
  [binary-numbers]
  (reduce
   (fn [acc binary-num]
     (map (fn [m digit]
            (update m digit (fnil inc 0))) acc binary-num))
   (vec (repeat (count (first binary-numbers)) {}))
   binary-numbers))

(defn higher-frequency-digit
  [m]
  (if (> (get m 0 0)
         (get m 1 0))
    0
    1))

(defn lower-frequency-digit
  [m]
  (if (< (get m 0 0)
         (get m 1 0))
    0
    1))

(def challenge-input (->> "inputs/day-03.txt"
                          (io/resource)
                          slurp
                          input-str->binary-nums))

(defn power-consumption
  [binary-nums]
  (let [freqs (digit-frequencies binary-nums)
        gamma (->> freqs
                   (mapv higher-frequency-digit)
                   binary-vec->decimal)
        epsilon (->> freqs
                     (mapv lower-frequency-digit)
                     binary-vec->decimal)]
    {:gamma gamma
     :epsilon epsilon
     :power-consumption (* gamma epsilon)}))

(comment
  (power-consumption (input-str->binary-nums sample-input-str))
  (power-consumption challenge-input))

