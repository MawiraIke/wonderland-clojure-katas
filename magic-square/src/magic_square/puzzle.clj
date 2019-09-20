(ns magic-square.puzzle)

;(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])
(def values [[1.0 1.5 2.0]
             [2.5 3.0 3.5]
             [4.0 4.5 5.0]])

(def current-values (atom [[0 0 0]
                           [0 0 0]
                           [0 0 0]]))

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
  (let [num-a (get current-loc 0)
        num-b (get current-loc 1)]
    [(add-a num-a) (add-b num-b)]))

(defn add-elem-1 [values]
  (cond
    (= 3 (.length values))
    (let [n (get-in values [0 1])]
      (replace {n 1.0} (get values 0)))

    :else
    (println "Operations only supports 3*3 matrices")))

(defn magic-k [values]
  (let [n (.length values)]
    (* n (/ (+ (* n n) 1) 2))))
