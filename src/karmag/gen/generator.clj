(ns karmag.gen.generator
  (:require [clojure.walk :refer [walk]]
            [karmag.gen.random :as random]
            [karmag.gen.protocol :refer :all])
  (:import java.util.Random))

(defn- map-vals [m f]
  (reduce (fn [m [k v]] (assoc m k (f v))) m m))

(defn- find-next [gen pred]
  (if (exhausted? gen)
    gen
    (let [value (elem gen)]
      (if (pred value)
        gen
        (recur (step gen) pred)))))

(defn- realize [gen amount]
  (loop [gen gen
         left amount
         result (java.util.ArrayList. ^int amount)]
    (if (or (zero? left) (exhausted? gen))
      [gen result]
      (do (.add result (elem gen))
          (recur (step gen) (dec left) result)))))

(defn- make-random-elements [rng gen amount]
  (let [[gen elements] (realize gen amount)
        [seed rng] (random/next-long rng)
        java-random (java.util.Random. seed)]
    (java.util.Collections/shuffle elements java-random)
    [rng gen (vec elements)]))

(defn- at-generator [data f]
  (if (satisfies? Generator data)
    (f data)
    (walk #(at-generator % f) identity data)))

(defrecord ConstGen [value]
  Generator
  (reset [this] this)
  (exhausted? [this] false)
  (elem [this] value)
  (step [this] this))

(defrecord SeqGen [create-seq active]
  Generator
  (reset [this] (assoc this :active (create-seq)))
  (exhausted? [this] (empty? active))
  (elem [this] (first active))
  (step [this] (update-in this [:active] next)))

(defrecord CycleGen [gen]
  Generator
  (reset [this] (assoc this :gen (reset gen)))
  (exhausted? [this] (and (exhausted? gen)
                          (-> gen reset exhausted?)))
  (elem [this] (elem gen))
  (step [this] (let [new-gen (step gen)]
                 (if (exhausted? new-gen)
                   (reset this)
                   (assoc this :gen new-gen)))))

(defrecord MapGen [gen f]
  Generator
  (reset [this] (assoc this :gen (reset gen)))
  (exhausted? [this] (exhausted? gen))
  (elem [this] (f (elem gen)))
  (step [this] (assoc this :gen (step gen))))

(defrecord MappedGen [gen-map]
  Generator
  (reset [this] (assoc this :gen-map (map-vals gen-map reset)))
  (exhausted? [this] (->> gen-map vals (some exhausted?)))
  (elem [this] (map-vals gen-map elem))
  (step [this] (assoc this :gen-map (map-vals gen-map step))))

(defrecord FilterGen [gen pred]
  Generator
  (reset [this] (update-in this [:gen] (comp #(find-next % pred) reset)))
  (exhausted? [this] (exhausted? (find-next gen pred)))
  (elem [this] (elem gen))
  (step [this] (update-in this [:gen] (comp #(find-next % pred) step))))

(defrecord ShuffleGen [gen block-size seed rng elements]
  Generator
  (reset [this]
    (let [rng (random/create seed)
          gen (reset gen)
          [rng gen elements] (make-random-elements rng gen block-size)]
      (assoc this :gen gen :rng rng :elements elements)))
  (exhausted? [this] (and (empty? elements)
                          (exhausted? gen)))
  (elem [this] (first elements))
  (step [this]
    (if (< 1 (count elements))
      (update-in this [:elements] next)
      (let [[rng gen elements] (make-random-elements rng gen block-size)]
        (assoc this :gen gen :rng rng :elements elements)))))

(defrecord WeightedGen [n-gen-coll active n gen]
  Generator
  (reset [this]
    (let [n-gen-coll (map (fn [[n gen]] [n (reset gen)]) n-gen-coll)
          [[n gen] & _] n-gen-coll]
      (assoc this :active n-gen-coll :n n :gen gen)))
  (exhausted? [this] (exhausted? gen))
  (elem [this] (elem gen))
  (step [this]
    (if (= n 1)
      (let [original-n (-> active first first)
            active (concat (drop 1 active) [[original-n (step gen)]])
            [[n gen & _]] active]
        (assoc this :active active :n n :gen gen))
      (assoc this :n (dec n) :gen (step gen)))))

(defrecord ArbitraryGen [data]
  Generator
  (reset [this]
    (update this :data at-generator reset))
  (exhausted? [this]
    (let [result (atom false)]
      (at-generator data #(when (exhausted? %)
                            (reset! result true)
                            %))
      @result))
  (elem [this]
    (at-generator data elem))
  (step [this]
    (update this :data at-generator step)))

(defrecord IterateGen [original-state state f done-f head?]
  Generator
  (reset [this] (assoc this :state original-state :head? true))
  (exhausted? [this] (done-f state))
  (elem [this] (if head? state (f state)))
  (step [this]
    (if head?
      (assoc this :head? false)
      (assoc this :state (f state)))))
