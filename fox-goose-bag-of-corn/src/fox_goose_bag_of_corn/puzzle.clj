(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[:fox :goose :corn :you] [:boat] []])       ;:you

(def current-pos (atom start-pos))

(def end-pos [[] [:boat] [:fox :goose :corn :you]])

(def boat-capacity 3)

(defn check-friendliness
  "Check whether two items are friendly, e.g. The fox and the goose are not"
  [item-1 item-2]
  (if (= item-1 :fox)
    (if (= item-2 :goose)
      false
      true)
    (if (= item-1 :goose)
      (if (= item-2 :corn)
        false
        true)
      true)))

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
                           (assoc new-vec 2 (conj (get new-vec 2) elem :you)))
                ;Print >>  [[:goose] [:boat] [:fox :corn :you]]
                remove-user (fn [init-vec]
                              (mapv vec (map #(disj (set %) :you) init-vec)))
                return-user (fn [init-vec]
                              (assoc init-vec 0 (conj (get init-vec 0) :you)))]
            (->> @current-pos
                 (remove-element)
                 (do-swap)
                 (add-elem)
                 (do-swap)
                 (remove-user)
                 (return-user)
                 (do-swap))))
  ([elem-1 elem-2] (let [remove-element (fn [init-vec]
                                          (mapv vec (map #(disj (set %) elem-1 elem-2 :you) init-vec)))
                         do-swap (fn [new-vec]
                                   (reset! current-pos new-vec))
                         add-elem (fn [new-vec]
                                    (assoc new-vec 2 (conj (get new-vec 2) elem-1 elem-2 :you)))
                         ;Print >>  [[:goose] [:boat] [:fox :corn :you]]
                         remove-user (fn [init-vec]
                                       (mapv vec (map #(disj (set %) :you) init-vec)))
                         return-user (fn [init-vec]
                                       (assoc init-vec 0 (conj (get init-vec 0) :you)))]
                     (->> @current-pos
                          (remove-element)
                          (do-swap)
                          (add-elem)
                          (do-swap)
                          (remove-user)
                          (return-user)
                          (do-swap)))))

(defn perform-move2
  "This move is called to perform a move"
  ([elem initial-vector] (let [remove-element (fn [init-vec]
                                                ;Thanks to https://stackoverflow.com/questions/57591009/converting-a-set-to-a-vector-results-in-a-vector-of-nested-vectors-in-clojure/57591310#57591310
                                                (mapv vec (map #(disj (set %) elem :you) init-vec)))
                               add-elem (fn [new-vec]
                                          (assoc new-vec 2 (conj (get new-vec 2) elem :you)))
                               ;Print >>  [[:goose] [:boat] [:fox :corn :you]]
                               remove-user (fn [init-vec]
                                             (mapv vec (map #(disj (set %) :you) init-vec)))
                               return-user (fn [init-vec]
                                             (assoc init-vec 0 (conj (get init-vec 0) :you)))]
                           (->> initial-vector
                                (remove-element)
                                (add-elem)
                                (remove-user)
                                (return-user))))
  ([elem-1 elem-2 initial-vector] (let [remove-element (fn [init-vec]
                                                         (mapv vec (map #(disj (set %) elem-1 elem-2 :you) init-vec)))
                                        add-elem (fn [new-vec]
                                                   (assoc new-vec 2 (conj (get new-vec 2) elem-1 elem-2 :you)))
                                        ;Print >>  [[:goose] [:boat] [:fox :corn :you]]
                                        remove-user (fn [init-vec]
                                                      (mapv vec (map #(disj (set %) :you) init-vec)))
                                        return-user (fn [init-vec]
                                                      (assoc init-vec 0 (conj (get init-vec 0) :you)))]
                                    (->> initial-vector
                                         (remove-element)
                                         (add-elem)
                                         (remove-user)
                                         (return-user)))))

;TODO CHECK HOW TO RECUR THIS
;Should work fine for method one of solution
;This is where only one formulae is applied,
;It should fetch all the ways and show them and show method 1 as the fastest formulae
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
            (if (check-friendliness elem-1 elem-2)
              (perform-move elem-1 elem-2)
              (if (check-friendliness elem-1 elem-3)
                (perform-move elem-1 elem-3)
                (if (check-friendliness elem-2 elem-3)
                  (perform-move elem-2 elem-3)
                  ()))))
        4 (let [remove-element (fn [init-vec]
                                 ;Thanks to https://stackoverflow.com/questions/57591009/converting-a-set-to-a-vector-results-in-a-vector-of-nested-vectors-in-clojure/57591310#57591310
                                 (mapv vec (map #(disj (set %) :you) init-vec)))
                swap-elements (fn [vec1]
                                (reset! current-pos vec1))]
            (->> @current-pos
                 ;(remove-element)
                 (swap-elements)
                 (let [elem-1 (get side-a 0)
                       elem-2 (get side-a 1)
                       elem-3 (get side-a 2)]
                   (if (check-friendliness elem-1 elem-2)
                     (do
                       (perform-move elem-1 elem-2)
                       ;(recur)
                       )
                     (if (check-friendliness elem-1 elem-3)
                       (do
                         (perform-move elem-1 elem-3)
                         ;(recur)
                         )
                       (if (check-friendliness elem-2 elem-3)
                         (do
                           (perform-move elem-2 elem-3)
                           ;(recur)
                           ) ()))))))))))

(defmacro do-until [& clauses]
  (when clauses
    (list `when (first clauses)
          (if (next clauses)
            (second clauses)
            (throw (IllegalArgumentException.
                     "do-until requires an even number of forms")))
          (cons 'do-until (nnext clauses)))))

;
;Get vector
;loop through elements, one each time

(defn river-crossing-plan2 []
  (let [side-a (get @current-pos 0)
        side-b (get @current-pos 1)
        side-c (get @current-pos 2)
        swap-elements (fn [vec1]
                        (reset! current-pos vec1))]

    (do-until
      (= 4 (.length (get @current-pos 0))) (->> @current-pos
                                                ;(remove-element)
                                                (swap-elements)
                                                (let [elem-1 (get side-a 0)
                                                      elem-2 (get side-a 1)
                                                      elem-3 (get side-a 2)]
                                                  (if (check-friendliness elem-1 elem-2)
                                                    (perform-move elem-1 elem-2)
                                                    (if (check-friendliness elem-1 elem-3)
                                                      (perform-move elem-1 elem-3)
                                                      (if (check-friendliness elem-2 elem-3)
                                                        (perform-move elem-2 elem-3)
                                                        (println "Unmovable items"))))))
      (= 3 (.length (get @current-pos 0))) (->> @current-pos
                                                (let [elem-1 (get (get @current-pos 0) 0)
                                                      elem-2 (get (get @current-pos 0) 1)
                                                      elem-3 (get (get @current-pos 0) 2)]
                                                  (if (check-friendliness elem-1 elem-2)
                                                    (perform-move elem-1 elem-2)
                                                    (if (check-friendliness elem-1 elem-3)
                                                      (perform-move elem-1 elem-3)
                                                      (if (check-friendliness elem-2 elem-3)
                                                        (perform-move elem-2 elem-3)
                                                        (println "Unmovable items")))))
                                                (swap-elements))
      (= 2 (.length (get @current-pos 0))) (@current-pos
                                             (let [elem-1 (get (get @current-pos 0) 0)
                                                   elem-2 (get (get @current-pos 0) 1)]
                                               (if [= (check-friendliness elem-1 elem-2) :true]
                                                 (perform-move elem-1 elem-2)
                                                 ()))
                                             (swap-elements))
      (= 1 (.length (get @current-pos 0))) (let [elem-1 (get (get @current-pos 0) 0)]
                                             (perform-move elem-1))
      (= 0 (.length (get @current-pos 0))) (print (str "Game over, you have finally moved all items" @current-pos))
      :lollipop (println "Truthy thing"))))

;163