
(def characters {
 \T [
 [0 1 1 1 1 1 0 0]
 [0 0 0 1 0 0 0 0] 
 [0 0 0 1 0 0 0 0]
 [0 0 0 1 0 0 0 0]
 [0 0 0 1 0 0 0 0]
 [0 0 0 1 0 0 0 0]
 [0 0 0 1 0 0 0 0]
 [0 0 0 1 0 0 0 0]
 ]

 \H [
 [0 1 0 0 0 1 0 0]
 [0 1 0 0 0 1 0 0]
 [0 1 0 0 0 1 0 0]
 [0 1 1 1 1 1 0 0]
 [0 1 0 0 0 1 0 0]
 [0 1 0 0 0 1 0 0]
 [0 1 0 0 0 1 0 0]
 [0 1 0 0 0 1 0 0]
 ]

 \Y [
 [0 1 0 0 0 1 0 0]
 [0 0 1 0 1 0 0 0] 
 [0 0 0 1 0 0 0 0]
 [0 0 0 1 0 0 0 0]
 [0 0 0 1 0 0 0 0]
 [0 0 0 1 0 0 0 0]
 [0 0 0 1 0 0 0 0]
 [0 0 0 1 0 0 0 0]
 ]

 \space  [
 [0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0] 
 [0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0]
 [0 0 0 0 0 0 0 0]
 ]

})

  
(defn transpose
  "Switches rows and columns"
  [char-matrix]
  (apply map vector char-matrix))

(defn byte-value
  [vec]
  (apply + (map (fn [v pos] (bit-shift-left v pos)) vec (reverse (range 0 8)))))

(defn to-bytes
  [char-matrix]
  (map byte-value (transpose char-matrix)))

(defn string-to-bytes
  [text]
  (vec (flatten (map to-bytes (map characters text)))))


(defn rotate
  ([s] (rotate s 0))
  ([s idx]
     (lazy-seq (cons (nth s (mod idx (count s))) (rotate s (inc idx))))))



