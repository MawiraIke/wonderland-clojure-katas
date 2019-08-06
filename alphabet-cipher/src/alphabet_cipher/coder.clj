(ns alphabet-cipher.coder)

(def alphabet (seq "abcdefghijklmnopqrstuvwxyz"))

;Repeat list of alphabets as a vector
(def repeatedList (vec (take 26 (apply concat (repeat alphabet)))))

(defn generateKeyword
  "Generate the keyword"
  [keyword, len]
  (vec (seq (char-array (take len (apply concat (repeat keyword)))))))

(defn getValueAtIndex
  "Get the value at an index in collection"
  [index collection]
  (nth collection index))

(defn getIndexOfChar
  "Get the index of a value in a collection"
  [character collection]
  (.indexOf collection character))

(defn truncate-list
  "Return a list of alphabets that is truncated to form a row of the lookup table"
  [character]
  (let [num (getIndexOfChar character repeatedList)]
    (subvec (vec (take (+ num 26) (apply concat (repeat alphabet)))) num (+ num 26))))

(defn get-intersec
  "Gets the letter at the intersection between a row and a column of a lookup table"
  [char1 len index1 keyWord]
  (let [vec1 (truncate-list char1)                          ;Generate row
        vec2 (generateKeyword keyWord len)
        char2 (getValueAtIndex index1 vec2)
        index2 (getIndexOfChar char2 repeatedList)]
    (getValueAtIndex index2 vec1)))

(defn get-intersec2
  "Gets the letter at the intersection between a row and a column of a lookup table"
  [char1 len index1 keyWord]
  (let [vec1 (generateKeyword keyWord len)
        char2 (getValueAtIndex index1 vec1)
        vec2 (truncate-list char2)
        index2 (getIndexOfChar char1 vec2)]
    (getValueAtIndex index2 repeatedList)))

(defn encode
  "Encodes a word"
  [keyWord encode-text]
  (let [encode-vec (vec encode-text)
        loop-count (atom 0)]
    (for [i encode-vec]
      (do (print (get-intersec i (.length encode-vec) @loop-count keyWord))
          (swap! loop-count inc)))))

(defn decode
  "Decodes a decoded word"
  [keyWord decode-text]
  (let [decode-vec (vec decode-text)
        loop-count (atom 0)]
    (for [i decode-vec]
      (do
        (print (get-intersec2 i (.length decode-vec) @loop-count keyWord))
        (swap! loop-count inc)))))