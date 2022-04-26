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
  "Given input like
    [[1 0 1]
      [1 0 0]]
   The output will be
     [{1 2} {0 2} {0 1, 1 1}]
   Each element of the output is a map with maximum two keys, 0 and 1.
    The value of each key is the frequency of that digit in that bit-location
    in all the binary numbers. In the most-significant bit-location,
    the digit 1 appears twice, hence the output contains the map {1 2}"
  [binary-numbers]
  (reduce
   (fn [acc binary-num]
     (map (fn [m digit]
            (update m digit (fnil inc 0)))
          acc
          binary-num))
   (vec (repeat (count (first binary-numbers)) {}))
   binary-numbers))

(defn higher-frequency-digit
  "Given a map of digit frequences, returns the digit
  that is more frequent. Eg for input {0 3, 1 4}, return 1"
  [m]
  (if (> (get m 0 0)
         (get m 1 0))
    0
    1))

(defn lower-frequency-digit
  "Given a map of digit frequences, returns the digit
  that is less frequent. Eg for input {0 3, 1 4}, return 0"
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

(defn most-common-bit
  [bit-position binary-nums]
  (let [freqs (frequencies (map #(nth %1 bit-position) binary-nums))]
    (if (< (get freqs 1 0)
           (get freqs 0 0))
      0
      1)))

(defn least-common-bit
  [bit-position binary-nums]
  (let [freqs (frequencies (map #(nth %1 bit-position) binary-nums))]
    (if (<= (get freqs 0 0)
            (get freqs 1 0))
      0
      1)))

(defn oxygen-generator-rating
  ([binary-nums]
   (loop [bit-position 0
          binary-nums binary-nums]
     (cond
       (= 0 (count binary-nums)) :invalid-input-empty
       (= 1 (count binary-nums)) (first binary-nums)
       (<= (count (first binary-nums)) bit-position) :not-found
       :else (let [bit-criteria (most-common-bit bit-position binary-nums)]
               (recur (inc bit-position)
                      (filter #(= bit-criteria (nth %1 bit-position))
                              binary-nums)))))))

(defn co2-scrubber-rating
  ([binary-nums]
   (loop [bit-position 0
          binary-nums binary-nums]
     (cond
       (= 0 (count binary-nums)) :invalid-input-empty
       (= 1 (count binary-nums)) (first binary-nums)
       (<= (count (first binary-nums)) bit-position) :not-found
       :else (let [bit-criteria (least-common-bit bit-position binary-nums)]
               (recur (inc bit-position)
                      (filter #(= bit-criteria (nth %1 bit-position))
                              binary-nums)))))))

(comment
  (power-consumption (input-str->binary-nums sample-input-str))
  (power-consumption challenge-input)


  (most-common-bit 0 [[1 0 0][0 1 1]])
  (oxygen-generator-rating [[0 1 0][1 1 1][0 1 1]])
  (oxygen-generator-rating (input-str->binary-nums sample-input-str))
  (co2-scrubber-rating (input-str->binary-nums sample-input-str)))
