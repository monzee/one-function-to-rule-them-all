(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (reduce #(conj %1 x %2) [(first a-seq)] (rest a-seq))))

(defn my-count [a-seq]
  (reduce (fn [n _] (inc n)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) '() a-seq))

(defn min-max-element [a-seq]
  (let [x (first a-seq)
        xs (rest a-seq)]
    (reduce (fn [[low hi] n] [(min low n) (max hi n)]) [x x] xs)))

(defn insert [sorted-seq n]
  (if (empty? sorted-seq)
    (list n)
    (let [[x & xs] sorted-seq]
      (if (<= n x)
        (cons n sorted-seq)
        (cons x (insert xs n))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce #(if (contains? %1 %2)
             (disj %1 %2)
             (conj %1 %2)) #{} a-seq))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& xs]
  (count xs))

(defn my-* [& xs]
  (reduce * 1 xs))

(defn pred-and [& preds]
  (fn [x]
    (loop [[p & ps :as preds] preds]
      (if (empty? preds)
        true
        (and (p x)
             (recur ps))))))

(defn my-map [f & xss]
  (if (some empty? xss)
    '()
    (->> (map first xss)
         (apply f)
         (conj (apply my-map (cons f (map rest xss)))))))
