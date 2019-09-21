(ns magic-square.puzzle)

;(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])
(def values [[1.0 1.5 2.0]
             [2.5 3.0 3.5]
             [4.0 4.5 5.0]])

(def current-values (atom [[nil nil nil]
                           [nil nil nil]
                           [nil nil nil]]))

(defn magic-square [values]
  [[1.0 1.5 2.0]
   [2.5 3.0 3.5]
   [4.0 4.5 5.0]])

(defn add-a [a]
  (cond
    (= 0 a)
    2

    (= 1 a)
    0

    (= 2 a)
    1))

(defn add-b [b]
  (cond
    (= 0 b)
    1

    (= 1 b)
    2

    (= 2 b)
    0))

(defn add-exception [[a b]]
  (cond
    (= 0 a)
    [1 b]

    (= 1 a)
    [2 b]

    (= 2 a)
    [0 b]))

(defn add-logic [current-loc]
  "Expects a location vector, returns the next space to fill up"
  (let [num-a (get current-loc 0)
        num-b (get current-loc 1)
        current-loc-2 [(add-a num-a) (add-b num-b)]]
    (if (= (get-in @current-values current-loc-2) nil)
      current-loc-2
      (add-exception current-loc)                                                  ;Space is already filled up
      )))

(defn replace-func
  "Replaces a vector in a 3*3 matrix"
  ([values] (into [] (cons (get-in values [0 0]) (conj [1.0] (get-in values [0 2])))))
  ([[k v] r] (cond
               (= v 0)
               (into [] (cons r (conj [(get-in @current-values [k 1])] (get-in @current-values [k 2]))))
               (= v 1)
               (into [] (cons (get-in @current-values [k 0]) (conj [r] (get-in @current-values [k 2]))))
               (= v 2)
               (into [] (cons (get-in @current-values [k 0]) (conj [(get-in @current-values [k 1])] r)))
               ;(into [] (cons (get-in @current-values [2 0]) (get-in @current-values [2 1])))
               ))
  ([values [r d] f] (cond
                      (= d 0)
                      (conj [] (get values 0) (get values 1) (replace-func (add-logic [r d]) f))
                      (= d 1)
                      (conj [] (get values 0) (replace-func (add-logic [r d]) f) (get values 1))
                      (= d 2)
                      (conj [] (get values 0) (get values 1) (replace-func (add-logic [r d]) f))))


  ([values [r d] [t g] f] (cond
                            (= t 0)
                            (conj [] (replace-func [t g] f) (get values 1) (get values 2))
                            (= t 1)
                            (conj [] (get values 0) (replace-func [t g] f) (get values 2))
                            (= t 2)
                            (conj [] (get values 0) (get values 1) (replace-func [t g] f)))))

(defn add-loop []
  ;(replace {n (replace-func (add-logic [0 1]) 2)} orig)
  (let [current-loc (atom [0 1])]
    (do
      (reset!
        current-values
        (conj [] (replace-func @current-values) (get @current-values 1) (get @current-values 2)))
      (reset! current-values (replace-func @current-values @current-loc (add-logic @current-loc) 2.0))
      (reset! current-values (replace-func @current-values [2 2] (add-logic [2 2]) 3.0))
      (reset! current-values (replace-func @current-values [1 0] (add-logic [1 0]) 4.0))
      (reset! current-values (replace-func @current-values [2 0] (add-logic [2 0]) 5.0))
      (reset! current-values (replace-func @current-values [1 1] (add-logic [1 1]) 6.0))
      (reset! current-values (replace-func @current-values [0 2] (add-logic [0 2]) 7.0))
      (reset! current-values (replace-func @current-values [1 2] (add-logic [1 2]) 8.0))
      (reset! current-values (replace-func @current-values [0 0] (add-logic [0 0]) 9.0))
      )))

(defn magic-k [values]
  (let [n (.length values)]
    (* n (/ (+ (* n n) 1) 2))))
