(ns aoc-2021.day-02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def initial-state {:distance 0 :depth 0 :aim 0})

(defn parse-command-input
  [input]
  (let [parts (str/split input #" ")]
    (case (first parts)
      "forward" {:command :forward
                 :value (parse-long (second parts))}
      "up" {:command :up
            :value (parse-long (second parts))}
      "down" {:command :down
              :value (parse-long (second parts))}
      (ex-info "Unknown command" {:input input}))))

(defn execute-command
  [{:keys [command value]} state]
  (case command
    :forward (-> state
                 (update :distance (fnil + 0) value)
                 (update :depth (fnil + 0) (* (:aim state) value)))
    :down (update state :aim (fnil + 0) value)
    :up (update state :aim (fnil - 0) value)
    (ex-info "Unknown command" {:command command :value value})))

(def sample-input-str "forward 5
down 5
forward 8
up 3
down 8
forward 2")

(defn input-str->commands-seq
  [input-str]
  (->> input-str
       (str/split-lines)
       (mapv parse-command-input)))

(defn process-commands
  [commands state]
  (reduce (fn [state command]
            (execute-command command state))
          state
          commands))

(def challenge-input (->> "inputs/day-02.txt"
                          (io/resource)
                          slurp
                          input-str->commands-seq))

(process-commands challenge-input initial-state)

(comment
  (update initial-state :distance (fnil + 0) 3)

  (parse-command-input "up 4")
  (parse-command-input "forward 3")
  (parse-command-input "down 6")

  (execute-command
   (parse-command-input "forward 3")
   initial-state)

  (execute-command
   (parse-command-input "down 3")
   initial-state)

  (execute-command
   (parse-command-input "up 3")
   initial-state)

  (input-str->commands-seq sample-input-str)

  (process-commands [(parse-command-input "forward 3")
                     (parse-command-input "down 5")
                     (parse-command-input "forward 7")
                     (parse-command-input "up 3")]
                    initial-state))