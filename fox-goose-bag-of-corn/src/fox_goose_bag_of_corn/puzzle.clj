(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[:fox :goose :corn :you] [:boat] []])       ;:you

(def current-pos (atom start-pos))

(def end-pos [[] [:boat] [:fox :goose :corn :you]])

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

(defn three-item-logic [elements]
  "Function that receives three characters
  vector and finds which to move"
  (let [elem-1 (get elements 0)
        elem-2 (get elements 1)
        elem-3 (get elements 2)]
    (if (check-friendliness elem-1 elem-2)
      elem-3
      (if (check-friendliness elem-1 elem-3)
        elem-2
        (if (check-friendliness elem-2 elem-3)
          elem-1
          :List-not-supported)))))

(defn get-index [data item]
  (let [n (count (first data))
        i (.indexOf (flatten data) item)]
    (if (pos? i)
      (list (quot i n) (mod i n)))))

(defn river-crossing-plan [current-state]
  "Function that receives two characters
  vector and finds which to move"
  (let [side-b (fn [current-state]
                 (let [current-state @current-pos]
                   (if (empty? (get current-state 2))
                     (let [elem-1 (get-in current-state [1 1])
                           remove-element (fn [init-vec]
                                            (mapv vec (map #(disj (set %) elem-1 :you) init-vec)))
                           add-elem (fn [new-vec]
                                      (assoc new-vec 2 (conj (get new-vec 2) elem-1 :you)))
                           perform-swap (fn [new-vec]
                                          (reset! current-pos new-vec))]

                       (->> current-state
                            (remove-element)
                            (add-elem)
                            (perform-swap)
                            (println @current-pos))
                       ())
                     (let [elem-1 (get-in current-state [1 1])
                           elem-2 (get-in current-state [2 0])
                           remove-element (fn [init-vec]
                                            (mapv vec (map #(disj (set %) elem-1 :you) init-vec)))
                           add-elem (fn [new-vec]
                                      (assoc new-vec 2 (conj (get new-vec 2) elem-1 :you)))
                           perform-swap (fn [new-vec]
                                          (reset! current-pos new-vec))
                           remove-element-2 (fn [init-vec]
                                              (mapv vec (map #(disj (set %) elem-2) init-vec)))
                           add-elem-2 (fn [new-vec]
                                        (assoc new-vec 1 (conj (get new-vec 1) elem-2)))]

                       (if (= 2 (.length (get @current-pos 2)))
                         (->> current-state
                              (remove-element)
                              (add-elem)
                              (perform-swap)
                              (println @current-pos))
                         (if (check-friendliness elem-1 elem-2)
                           (->> current-state
                                (remove-element)
                                (add-elem)
                                (perform-swap)
                                (println @current-pos))
                           (->> current-state
                                (remove-element)
                                (add-elem)
                                (remove-element-2)
                                (add-elem-2)
                                (perform-swap)
                                (println @current-pos))))))))

        perform-swap (fn [new-vec]
                       (reset! current-pos new-vec))
        side-a (fn [current-state]
                 (let [current-state @current-pos]
                   (cond
                     (= 4 (.length (get current-state 0)))
                     (let [elem-1 (three-item-logic [(get-in current-state [0 0]) (get-in current-state [0 1]) (get-in current-state [0 2])])
                           remove-element (fn [init-vec]
                                            ;Thanks to https://stackoverflow.com/questions/57591009/converting-a-set-to-a-vector-results-in-a-vector-of-nested-vectors-in-clojure/57591310#57591310
                                            (mapv vec (map #(disj (set %) elem-1 :you) init-vec)))
                           add-element (fn [new-vec]
                                         (assoc new-vec 1 (conj (get new-vec 1) elem-1)))]
                       (->> current-state
                            (remove-element)
                            (add-element)
                            (perform-swap)
                            ;Call side B here
                            (println @current-pos)))

                     (= 3 (.length (get current-state 0)))
                     (let [elem-1 (three-item-logic [(get-in current-state [0 0]) (get-in current-state [0 1]) (get-in current-state [0 2])])
                           remove-element (fn [init-vec]
                                            ;Thanks to https://stackoverflow.com/questions/57591009/converting-a-set-to-a-vector-results-in-a-vector-of-nested-vectors-in-clojure/57591310#57591310
                                            (mapv vec (map #(disj (set %) elem-1 :you) init-vec)))
                           add-element (fn [new-vec]
                                         (assoc new-vec 1 (conj (get new-vec 1) elem-1)))]
                       (->> current-state
                            (remove-element)
                            (add-element)
                            (perform-swap)
                            ;Call side B here
                            (println @current-pos)))

                     (= 2 (.length (get current-state 0)))
                     (let [elem-to-be-moved (get-in @current-pos [0 0])
                           remove-element (fn [elem-1]
                                            (mapv vec (map #(disj (set %) elem-to-be-moved :you) @current-pos)))
                           add-element (fn [new-vec]
                                         (assoc new-vec 1 (conj (get new-vec 1) elem-to-be-moved)))]

                       (->> current-state
                            (remove-element)
                            (add-element)
                            (perform-swap)
                            ;Call side B here
                            (println @current-pos)))

                     (= 1 (.length (get current-state 0)))
                     (let [elem-to-be-moved (get-in @current-pos [1 1])
                           length (get @current-pos 1)
                           elem-to-be-moved-2 (get-in @current-pos [0 0])
                           remove-element (fn [new-vec]
                                            (mapv vec (map #(disj (set %) elem-to-be-moved :you) new-vec)))
                           add-element (fn [new-vec]
                                         (assoc new-vec 0 (conj (get new-vec 0) elem-to-be-moved :you)))
                           remove-element-2 (fn [new-vec]
                                              (mapv vec (map #(disj (set %) elem-to-be-moved-2 :you) new-vec)))
                           add-element-2 (fn [new-vec]
                                           (assoc new-vec 1 (conj (get new-vec 1) elem-to-be-moved-2)))]

                       (if (< (.length length) 2)
                         (->> current-state
                              (remove-element-2)
                              (add-element-2)
                              (perform-swap)
                              (println @current-pos))       ;Last round of moves
                         (->> current-state
                              (remove-element)
                              (add-element)
                              (remove-element-2)
                              (add-element-2)
                              (perform-swap)
                              (println @current-pos))))     ;First time you come to side A after leaving

                     :else
                     (println "End of sequence"))))
        iter (fn [state]
               (->>
                 (side-a)
                 (side-b)))]

    (cond (= start-pos @current-pos)
          (when (final-move? @current-pos)
            (println @current-pos)
            (recur (iter @current-pos)))
          ;(->> current-state
          ;     ;(loop [stat current-state]
          ;     ;  (if (final-move? @current-state)
          ;     ;    ("End game")
          ;     ;    (->>
          ;     ;      (side-a)
          ;     ;      (side-b)
          ;     ;      ;(recur )
          ;     ;      )))
          ;
          ;     ;(side-a)                                     ;Does not continue because the println statements in side A println null
          ;     ;(side-b)
          ;     ;(side-a)
          ;     ;(side-b)
          ;     ;(side-a)
          ;     ;(side-b)
          ;     ;(side-a)
          ;     ;(side-b)
          ;     )
    )))

;192