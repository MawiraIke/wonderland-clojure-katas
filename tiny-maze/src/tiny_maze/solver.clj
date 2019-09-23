(ns tiny-maze.solver)

(def maze [[:S 0 1]
           [1 0 1]
           [1 0 :E]])

(defn append-char [[a b] char]
  (cond
    (= a 0)
    (cond
      (= b 0)
      (conj [] (conj [] char (get-in maze [a 1]) (get-in maze [a 2])) (get maze 1) (get maze 2))
      (= b 1)
      (conj [] (conj [] (get-in maze [a 0]) char (get-in maze [a 2])) (get maze 1) (get maze 2))
      (= b 2)
      (conj [] (conj [] (get-in maze [a 0]) (get-in maze [a 1]) char) (get maze 1) (get maze 2)))

    (= a 1)
    (cond
      (= b 0)
      (conj [] (get maze 0) (conj [] char (get-in maze [a 1]) (get-in maze [a 2])) (get maze 2))
      (= b 1)
      (conj [] (get maze 0) (conj [] (get-in maze [a 0]) char (get-in maze [a 2])) (get maze 2))
      (= b 2)
      (conj [] (get maze 0) (conj [] (get-in maze [a 0]) (get-in maze [a 1]) char) (get maze 2)))

    (= a 2)
    (cond
      (= b 0)
      (conj [] (get maze 0) (get maze 1) (conj [] char (get-in maze [a 1]) (get-in maze [a 2])))
      (= b 1)
      (conj [] (get maze 0) (get maze 1) (conj [] (get-in maze [a 0]) char (get-in maze [a 2])))
      (= b 2)
      (conj [] (get maze 0) (get maze 1) (conj [] (get-in maze [a 0]) (get-in maze [a 1]) char)))
    ))

(defn next-pos [current-loc]
  (cond
    (not (= (get-in maze [(get current-loc 0) (+ (get current-loc 1) 1)]) 1))
    [(get current-loc 0) (+ (get current-loc 1) 1)]

    (not (= (get-in maze [(+ (get current-loc 0) 1) (get current-loc 1)]) 1))
    [(+ (get current-loc 0) 1) (get current-loc 1)]

    :else
    (println "End of the road")))


(defn solve-maze [loc]
  "Jumps to the next location"
  (loop [current-loc loc]
    (let [new-loc (next-pos current-loc)]
      (if (= :E (get-in maze new-loc))
        (println "Maze solved")
        (do
          (println (append-char new-loc :x))
          (recur new-loc))))))




