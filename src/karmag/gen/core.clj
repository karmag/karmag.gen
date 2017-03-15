(ns karmag.gen.core
  (:refer-clojure :exclude [map cycle filter shuffle iterate])
  (:require [karmag.gen.generator :as g]
            [karmag.gen.protocol :as prot]
            [karmag.gen.random :as random]))

(def reset prot/reset)
(def exhausted? prot/exhausted?)
(def elem prot/elem)
(def step prot/step)

(defn const
  "Generator that infinitely returns the same element."
  [value]
  (karmag.gen.generator.ConstGen. value))

(defn from-seq
  "Takes a sequence or function that returns a sequence and makes a
  generator that returns the same sequence of elements. The head
  element is held so infinite sequences are not recommended. Use the
  fn form for infinite sequences."
  [fn-or-seq]
  (if (fn? fn-or-seq)
    (karmag.gen.generator.SeqGen. fn-or-seq (fn-or-seq))
    (karmag.gen.generator.SeqGen. (constantly fn-or-seq) fn-or-seq)))

(defn map
  "Generator that wraps the given generator and applies f to each
  element."
  [gen f]
  (karmag.gen.generator.MapGen. gen f))

(defn cycle
  "Automatically restarts the generator when it becomes exhausted
  creating an infinite generator."
  [gen]
  (karmag.gen.generator.CycleGen. gen))

(defn mapped
  "Takes a map of {key gen} and produces {key (elem gen)}
  elements. This generator is exhausted when any sub-generator is."
  [key-gen-map]
  (karmag.gen.generator.MappedGen. key-gen-map))

(defn filter
  "Filters elements based on the given predicate."
  [gen pred]
  (if (pred (elem gen))
    (karmag.gen.generator.FilterGen. gen pred)
    (step (karmag.gen.generator.FilterGen. gen pred))))

(defn shuffle
  "Generator that will fetch :block-size elements from the underlaying
  generator and shuffle the order of those elements to form a new
  order. As elements are consumed more blocks will be fetched and
  shuffled."
  ([gen] (shuffle gen nil))
  ([gen opts]
   (let [{:keys [block-size seed]} (merge {:block-size 100, :seed 0} opts)]
     (step
      (karmag.gen.generator.ShuffleGen.
       gen block-size seed (random/create seed) nil)))))

(defn weighted
  "Takes a coll [[weight gen]]. For each pair the generator is queried
  weight times before moving on to the next pair. The entire coll will
  be cycled if the end is reached. This generator becomes exhausted
  when it encounters the first exhausted generator."
  [n-gen-coll]
  {:pre [(every? pos? (clojure.core/map first n-gen-coll))]}
  (let [[[n gen] & _] n-gen-coll]
    (karmag.gen.generator.WeightedGen. n-gen-coll n-gen-coll n gen)))

(defn arbitrary
  "Generator that takes an arbitrary clojure data structure. Elements
  in the data structure can be generators in which case they will be
  called when the corresponding generator function is called on this
  generator. This generator becomes exhausted when it encounters a
  generator in the data that is exhausted. If there are no generators
  it will never be exhausted."
  [data]
  (karmag.gen.generator.ArbitraryGen. data))

(defn iterate
  "Generates elements by calling f on state, the initial element is
  the state itself. The element is used as the next state. The
  option :done is used to indicate that the generator is exhausted,
  it's passed the state and should return a truthy value to
  indicate exhaustion."
  ([f state] (iterate f state nil))
  ([f state opts]
   (let [{:keys [done] :or {done (constantly false)}} opts]
     (karmag.gen.generator.IterateGen. state state f done true))))

(defn to-seq
  "Creates a lazy seq of elements taken from the given generator."
  [gen]
  (when (not (exhausted? gen))
    (lazy-cat [(elem gen)] (to-seq (step gen)))))
