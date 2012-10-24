(ns looping-is-recursion)

(defn power [base exp]
  (let [iter (fn [k acc]
               (if (zero? k)
                 acc
                 (recur (dec k) (* acc base))))]
    (iter exp 1)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq)      nil
    (= (count a-seq) 1) (first a-seq)
    :else               (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (loop [a-seq seq1
         b-seq seq2]
    (cond
      (and (empty? a-seq) (empty? b-seq)) true
      (or (empty? a-seq) (empty? b-seq))  false
      (= (first a-seq) (first b-seq))     (recur (rest a-seq) (rest b-seq))
      :else                               false)))

(defn find-first-index [pred a-seq]
  (loop [n 0
         a-seq a-seq]
    (cond
      (empty? a-seq)       nil
      (pred (first a-seq)) n
      :else                (recur (inc n) (rest a-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ sum n)
      (recur (+ sum (first a-seq)) (inc n) (rest a-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [a-set #{}
           a-seq a-seq]
      (if (empty? a-seq)
        a-set
        (recur (toggle a-set (first a-seq)) (rest a-seq))))))

(defn fast-fibo [n]
  (loop [a 0
         b 0
         c 1
         n n]
    (cond
      (<= n 0) 0
      (= n 1)  c
      :else    (recur b c (+ a b c) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [res []
         a-set #{}
         a-seq a-seq]
    (let [e (first a-seq)]
      (if (or (empty? a-seq) (contains? a-set e))
        res
        (recur (conj res e) (conj a-set e) (rest a-seq))))))
