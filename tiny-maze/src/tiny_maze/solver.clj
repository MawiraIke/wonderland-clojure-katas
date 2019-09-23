(ns tiny-maze.solver)

(defn get-index [character tensor]
  (map #(.indexOf % character) tensor))

(def maze [[:S 0 1]
           [1 0 1]
           [1 0 :E]])

(defn solve-maze [maze]
  (let [start (get-index :S maze)
        end (get-index :E maze)
        row-of-S (reduce #() start)
        key-of-S (disj start -1)
        key-of-E (disj end -1)
        index-of-S (get key-of-S 0)
        index-of-E (get key-of-E 0)]
    ))

(defn next-pos [current-loc]
  (cond
    (not (= (get-in maze [(get current-loc 0) (+ (get current-loc 1) 1)]) 1))
    [(get current-loc 0) (+ (get current-loc 1) 1)]

    (not (= (get-in maze [(+ (get current-loc 0) 1) (get current-loc 1)]) 1))
    [(+ (get current-loc 0) 1) (get current-loc 1)]

    :else
    (println "End of the road")))


(defn jumper [loc]
  "Jumps to the next location"
  (loop [current-loc loc]
    (let [new-loc (next-pos current-loc)]
      (if (= :E (get-in maze new-loc))
        (println "Maze solved")
        (recur new-loc)))))




