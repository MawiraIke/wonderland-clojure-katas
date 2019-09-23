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

(defn jumper [[a b]]
  "Jumps to the next location"
  (let [n1 (get-index :S maze)
        n2 (get-index :E maze)
        n4 0                                                ;Current location
        ]
    [n1 n2 n4]))




