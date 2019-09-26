(ns wonderland-number.finder)

(def ^:private wonderland_range
  "The range to search for a wonderland number"
  (range 100000 1000000))

(defn digits
  "Given a number returns the set of digits"
  [number]
  (set (str number)))

(defn has-same-digits?
  "Checks to see if two numbers have the same digits"
  [k v]
  (= (digits k) (digits v)))

(defn wonderland_number?
  "Checks to see if a number is the wonderland number. "
  [number])



(defn wonderland-number []
  ;; calculate me
  42)
