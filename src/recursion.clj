(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [leng1 (count seq-1)
        leng2 (count seq-2)]
        (if (> leng1 leng2)
          seq-1
          seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [filtered (my-filter pred? a-seq)]
    filtered))

(defn my-drop-while [pred? a-seq]
(if (empty? a-seq)
  a-seq
  (if (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (count a-seq) (count b-seq)) false
    (= (first a-seq) (first  b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (= 0 how-many-times)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
    (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons () ())
  (let [front (inits a-seq)
        back (tails a-seq)]
        (drop-last (map concat back front)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) 
    freqs
    (let [new-freqs (update freqs (first a-seq) inc)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (let [init (my-repeat (count a-seq) 0)
        freqs (into {} (map vector (set a-seq) init))]
    (my-frequencies-helper freqs a-seq)))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[k v] (first a-map)]
      (concat (my-repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (if (empty? a-seq)
  a-seq
  (let [mid-point (int (/ (count a-seq) 2))]
    (cons (my-take mid-point a-seq) (cons (my-drop mid-point a-seq) ())))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
      (if (< (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[fh sh] (halve a-seq)]
      (seq-merge (merge-sort fh) (merge-sort sh)))))

(defn resto [x y]
  (if (empty? x)
    y
    (resto (rest x) (rest y))))

(defn split-into-monotonics [a-seq]
    (cond 
      (empty? a-seq) a-seq
      (singleton? a-seq) (cons a-seq ())
    :else
      (let [split (inits a-seq)
            cres (last (my-take-while (fn [x] (or (seq= x (merge-sort x)) (seq= x (reverse (merge-sort x))))) split))]
        (cons cres (split-into-monotonics (resto cres a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])