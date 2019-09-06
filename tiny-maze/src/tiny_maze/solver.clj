(ns tiny-maze.solver)

(defn get-index [character tensor]
  (map #(.indexOf % character) tensor))

(defn solve-maze [maze]
  (let [start (get-index :S maze)
        end (get-index :E maze)
        row-of-S (reduce #() start)
        key-of-S (disj start -1)
        key-of-E (disj end -1)
        index-of-S (get key-of-S 0)
        index-of-E (get key-of-E 0)]
    ))



(def maze [[:S 0 1]
           [1 0 1]
           [1 0 :E]])

