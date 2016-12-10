(ns looping-is-recursion)

((defn power [base exp]
   (let [helper (fn [base exp real-base]
                 (if (= 1 exp)
                   base
                   (recur (* base real-base) (dec exp) real-base)))]
    ( if (= 0 exp) 1 (helper base exp base) ) )) 2 0)

((defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (if (= 1 (count a-seq))
                   (first a-seq)
                   (recur (rest a-seq))))]
    ( if (empty? a-seq) nil (helper a-seq) ) )) [])

((defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                  (if (and (empty? seq1) (empty? seq2)) true
                    (if (and (= (count seq1) (count seq2)) (= (first seq1) (first seq2)))
                      (recur (rest seq1) (rest seq2)) false) ))]
    (helper seq1 seq2)  )) [] [])

((defn find-first-index [pred a-seq]
   (loop [predi pred lista a-seq index 0]
    (cond
      (empty? lista) nil
      (predi (first lista)) index
      :else (recur predi (rest lista) (inc index))))) pos? [-2])


((defn avg [a-seq]
  (if (empty? a-seq) 0 (/ (apply + a-seq) (count a-seq)))) [1 2])


(defn lisaa-frekvenssi [frekvenssisetti item]
  (if (= (get frekvenssisetti item) nil) (assoc frekvenssisetti item 1) (assoc frekvenssisetti item (+ 1 (get frekvenssisetti item)))) )


((defn parity [a-seq] (loop [lista a-seq frekvenssisetti {}]
    (cond
      (empty? lista) (keys (filter #(odd? (mod (second %) 2)) frekvenssisetti))
      :else (recur (rest lista) (lisaa-frekvenssi frekvenssisetti (first lista)) ) ))) [1 1 2])



(defn fast-fibo [n]
  ( cond
   (= n 0) 0
    (= n 1) 1
    (= n 2) 1
    :else (loop [fnth 1 fnmiinus 1 ni n nykyinen 2]
      (cond
        (= nykyinen n) fnth
        :else (recur (+ fnth fnmiinus) fnth ni (+ nykyinen 1)))
    )))


(defn cut-at-repetition [a-seq]
  (loop [lista [] alkup a-seq]
    (if (empty? alkup) lista
     (recur (if (.contains lista (first alkup)) lista (conj lista (first alkup)) ) (rest alkup)))))

