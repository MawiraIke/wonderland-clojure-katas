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

(defn add-logic [current-loc]
  "Expects a location vector, returns the next space to fill up"
  (let [num-a (get current-loc 0)
        num-b (get current-loc 1)]
    [(add-a num-a) (add-b num-b)]))

(defn replace-func
  "Replaces the first vector middle location in a 3*3 matrix.
  This is the first step to solving a 3*3 matrix"
  ([values] (into [] (cons (get-in values [0 0]) (conj [1.0] (get-in values [0 2])))))
  ([[k v] r] (cond
               (= v 0)
               (into [] (cons r (conj (get-in values [k 1]) (get-in values [k 2]))))
               (= v 1)
               (into [] (cons (get-in values [k 0]) (conj [r] (get-in values [2]))))
               (= v 2)
               (into [] (cons (get-in values [k 0]) (conj (get-in values [k 1]) [r]))))))

(defn add-elems [values]
  (cond
    (= 3 (.length values))
    (let [n (get-in values [0 1])]
      ;(replace {n 1.0} (get values 0))
      (do
        (replace-func (get values 0))
        (replace-func (add-logic [0 1]) 2)))

    :else
    (println "Operations only supports 3*3 matrices")))

(defn magic-k [values]
  (let [n (.length values)]
    (* n (/ (+ (* n n) 1) 2))))
