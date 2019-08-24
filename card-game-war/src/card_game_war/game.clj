(ns card-game-war.game)

(def ^:private suits
  "The suit with their corresponding values."
  {:spades 1 :clubs 2 :diamonds 3 :hearts 4})

(def ^:private ranks
  "The possible ranks with their corresponding values."
  (zipmap [:two :three :four :five :six :seven :eight :nine :ten :jack :queen :king :ace] (range 2 15)))

(def ^:private cards
  "All possible card combinations represented as a map containing two keys: :rank and :suit.
   e.g. {:rank :two :suit :spade}"
  (for [suit suits rank ranks]
    {:suit (key suit) :rank (key rank)}))

(defn play-round
  "Takes two cards and compares them to find a winning card"
  [player-1-card player-2-card]
  (let [player-1-rank-value (get ranks (:rank player-1-card))
        player-2-rank-value (get ranks (:rank player-2-card))
        player-1-suit-value (get suits (:suit player-1-card))
        player-2-suit-value (get suits (:suit player-2-card))]
    (cond
      (> player-1-rank-value player-2-rank-value)
      :player-1
      (> player-2-rank-value player-1-rank-value)
      :player-2
      (> player-1-suit-value player-2-suit-value)
      :player-1
      (> player-2-suit-value player-1-suit-value)
      :player-2
      :else
      :player-1)))

(defn play-game
  ([]
   (let [shuffled-deck (shuffle cards)
         [player-1-cards player-2-cards] (split-at 26 shuffled-deck)]
     (play-game player-1-cards player-2-cards)))
  ([initial-player-1-cards initial-player-2-cards]
   (loop [player-1-cards initial-player-1-cards
          player-2-cards initial-player-2-cards
          round-number 1
          rounds []]
     (cond
       (empty? player-1-cards)
       (println "Player two wins the game!")

       (empty? player-2-cards)
       (println "Player one wins the game!")

       :else
       (let [player-1-card (first player-1-cards)
             player-2-card (first player-2-cards)
             winning-player (play-round player-1-card player-2-card)]
         (if (= winning-player :player-1)
           (recur (conj (rest player-1-cards) player-1-card player-2-card)
                  (rest player-2-cards)
                  (inc round-number)
                  ;(conj rounds (round-details round-number "Player one wins the round!" player-1-card player-2-card))
                  (println "Player one wins the round!" player-1-card " vs " player-2-card)
                  )
           (recur (rest player-1-cards)
                  (conj (rest player-2-cards) player-1-card player-2-card)
                  (inc round-number)
                  ;(conj rounds (round-details round-number "Player two wins the round!" player-1-card player-2-card))
                  (println "Player two wins the round!" player-2-card " vs " player-1-card)
                  )))))))

