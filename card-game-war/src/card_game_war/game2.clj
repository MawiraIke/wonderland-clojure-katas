(ns card-game-war.game2)


(def suits
  {:spades 1 :clubs 2 :diamonds 3 :hearts 4})

(def ranks
  (zipmap [:two :three :four :five :six :seven :eight :nine :ten :jack :queen :king :ace] (range 2 15)))

(def cards
  (for [suit suits rank ranks]
    {:suit (key suit) :rank (key rank)}))

(defn play-game
  ([]
    (let [shuffled-deck shuffle cards
          [player-1-cards player-2-cards] (split-at 26 shuffled-deck)]
      (play-game player-1-cards player-cards)))
  ([initial-player-1-cards initial-player-2-cards]
    (loop [player-1-cards initial-player-1-cards
           player-2-cards initial-player-2-cards
           round-number 1
           rounds []]
      )))