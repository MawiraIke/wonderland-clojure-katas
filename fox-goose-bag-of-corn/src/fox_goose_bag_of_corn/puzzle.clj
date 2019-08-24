(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[:fox :goose :corn] [:boat] []])            ;:you

(def current-pos (atom start-pos))

(def end-pos [[] [:boat] [:fox :goose :corn :you]])

(def boat-capacity 3)

(defn check-friendliness
  "Check whether two items are friendly, e.g. The fox and the goose are not"
  [item-1 item-2]
  (if (= item-1 :fox)
    (if (= item-2 :goose)
      :false
      :true)
    (if (= item-1 :goose)
      (if (= item-2 :corn)
        :false
        :true)
      :true)))

(defn final-move?
  "Checks to see if this is the final move"
  [move]
  (= end-pos move))

(defn perform-move
  "This move is called to perform a move"
  ([elem] (let [remove-element (fn [init-vec]
                                 ;Thanks to https://stackoverflow.com/questions/57591009/converting-a-set-to-a-vector-results-in-a-vector-of-nested-vectors-in-clojure/57591310#57591310
                                 (mapv vec (map #(disj (set %) elem :you) init-vec)))
                do-swap (fn [new-vec]
                          (reset! current-pos new-vec))
                add-elem (fn [new-vec]
                           (assoc new-vec 2 (conj (get new-vec 2) elem :you)))]
            (->> @current-pos
                 (remove-element)
                 (do-swap)
                 (add-elem))))
  ([elem-1 elem-2] (let [remove-element (fn [init-vec]
                                          (mapv vec (map #(disj (set %) elem-1 elem-2 :you) init-vec)))
                         do-swap (fn [new-vec]
                                   (reset! current-pos new-vec))
                         add-elem (fn [new-vec]
                                    (assoc new-vec 2 (conj (get new-vec 2) elem-1 elem-2 :you)))]
                     (->> @current-pos
                          (remove-element)
                          (do-swap)
                          (add-elem)))))

(defn river-crossing-plan []
  (loop [side-a (get @current-pos 0)                        ;Initial side
         side-b (get @current-pos 1)                        ;Boat
         side-c (get @current-pos 2)]                       ;End side
    (cond
      (final-move? current-pos)
      (println "Game-over, you successfully moved all the items!")

      :else
      (case (.length side-a)
        0 (print "No more items to move")
        1 (let [elem-1 (get side-a 0)]
            (perform-move elem-1))
        2 (let [elem-1 (get side-a 0)
                elem-2 (get side-a 1)]
            (if [= (check-friendliness elem-1 elem-2) :true]
              (perform-move elem-1 elem-2)
              ()))
        3 (let [elem-1 (get side-a 0)
                elem-2 (get side-a 1)
                elem-3 (get side-a 2)]
            (if [= (check-friendliness elem-1 elem-2) :true]
              (perform-move elem-1 elem-2)
              (if [= (check-friendliness elem-1 elem-3) :true]
                (perform-move elem-1 elem-3)
                (if [= (check-friendliness elem-2 elem-3) :true]
                  (perform-move elem-2 elem-3)
                  ()))))
        4 (;Contains :you,
            )))))
