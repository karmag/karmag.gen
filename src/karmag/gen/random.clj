(ns karmag.gen.random
  "Random generator. Adapted from the haskell standard library.")

(defn create [seed]
  (let [s (bit-and seed Long/MAX_VALUE) ;; can use Integer/MAX_VALUE here
        q (long (/ s 2147483562))
        s1 (mod s 2147483562)
        s2 (mod q 2147483398)]
    [(inc s1) (inc s2)]))

(defn next-int [[s1 s2]]
  (let [k    (quot s1 53668)
        s1_  (- (* 40014 (- s1 (* k 53668))) (* k 12211))
        s1__ (if (neg? s1_) (+ s1_ 2147483563) s1_)

        k_   (quot s2 52774)
        s2_  (- (* 40692 (- s2 (* k_ 52774))) (* k_ 3791))
        s2__ (if (neg? s2_) (+ s2_ 2147483399) s2_)

        z  (- s1__ s2__)
        z_ (if (not (pos? z)) (+ z 2147483562) z)]
    [z_ [s1__ s2__]]))

(defn next-long [rng]
  (let [[a g1] (next-int rng)
        [b g2] (next-int g1)]
    [(+ (bit-shift-left a 32) b) g2]))

(defn next-boolean [rng]
  (let [[n rng] (next-int rng)]
    [(even? n) rng]))
