(ns karmag.gen.core-test
  (:require [clojure.test :refer :all]
            [karmag.gen.core :as gen :refer [reset exhausted? elem step]]))

(defn- realize [n gen]
  (loop [gen gen, remaining n, result []]
    (cond
      (zero? remaining) [gen result]
      (exhausted? gen) [gen result]
      :else (recur (step gen) (dec remaining) (conj result (elem gen))))))

(deftest const-gen-test
  (let [gen (gen/const 1)]
    (is (not (exhausted? gen)))
    (is (= 1 (elem gen)))
    (is (= (repeat 10 1)
           (second (realize 10 gen))))))

(deftest seq-gen-test
  (testing "from function"
    (let [gen (gen/from-seq #(range 10))
          [final nums] (realize 10 gen)]
      (is (not (exhausted? gen)))
      (is (exhausted? final))
      (is (= (range 10) nums))))
  (testing "from sequence"
    (let [gen (gen/from-seq (range 10))
          [final nums] (realize 10 gen)]
      (is (not (exhausted? gen)))
      (is (exhausted? final))
      (is (= (range 10) nums)))))

(deftest map-gen-test
  (let [gen (gen/map (gen/from-seq #(range 10)) (partial + 100))
        [final nums] (realize 10 gen)]
    (is (not (exhausted? gen)))
    (is (exhausted? final))
    (is (= (map #(+ 100 %) (range 10)) nums))))

(deftest cycle-gen-test
  (let [gen (gen/cycle (gen/from-seq #(range 3)))
        [final nums] (realize 10 gen)]
    (is (not (exhausted? gen)))
    (is (not (exhausted? final)))
    (is (= [0 1 2 0 1 2 0 1 2 0] nums))))

(deftest mapped-gen-test
  (let [gen (gen/mapped {:name (gen/const "qwe")
                         :age (gen/from-seq #(range 3))})
        [final values] (realize 3 gen)]
    (is (not (exhausted? gen)))
    (is (exhausted? final))
    (is (= (map #(hash-map :name "qwe" :age %) (range 3))
           values))))

(deftest filter-gen-test
  (let [gen (gen/filter (gen/from-seq #(range 10)) odd?)
        [final values] (realize 10 gen)]
    (is (not (exhausted? gen)))
    (is (exhausted? final))
    (is (= (filter odd? (range 10)) values))))

(deftest random-gen-test
  (let [gen (gen/random (gen/from-seq (range 10)))]
    (is (= [2 9 1 0 4 5 6 3 7 8] (gen/to-seq gen))))
  (testing "block-size"
    (let [gen (gen/random (gen/from-seq (range 10))
                          {:block-size 6})]
      (is (= (set (range 6)) (set (take 6 (gen/to-seq gen)))))
      (is (= (set (range 6 10)) (set (drop 6 (gen/to-seq gen)))))))
  (testing "seed"
    (is (not= (gen/random (gen/from-seq (range 10)) {:seed -1})
              (gen/random (gen/from-seq (range 10)) {:seed 1})))))

(deftest weighted-gen-test
  (let [gen (gen/weighted [[3 (gen/from-seq (range 99))]
                           [1 (gen/from-seq (range 100 999))]])
        [_ result] (realize 12 gen)]
    (is (= [0 1 2 100 3 4 5 101 6 7 8 102] result))))

(deftest to-seq-test
  (is (= (gen/to-seq (gen/from-seq (range 10)))
         (range 10)))
  (testing "infinite sequences"
    (is (= (take 100 (gen/to-seq (gen/const :value)))
           (repeat 100 :value)))))

(deftest generic-generator-properties
  (let [gens [(gen/const :value)
              (gen/from-seq (range 100))
              (gen/map (gen/from-seq (range 100)) (partial + 100))
              (gen/cycle (gen/from-seq (range 10)))
              (gen/mapped {:name (gen/const "qwe")
                           :age (gen/from-seq (range 100))})
              (gen/filter (gen/from-seq (range 100)) even?)
              (gen/random (gen/from-seq (range 100)) {:block-size 10})
              (gen/weighted [[3 (gen/const 11)] [1 (gen/const 22)]])]]
    (testing "reseting"
      (doseq [gen gens]
        (let [[new-gen elems] (realize 1000 gen)]
          (is (= elems (->> new-gen reset (realize 1000) second))))))
    (testing "immutable"
      (doseq [gen gens]
        (let [[_ elems] (realize 1000 gen)]
          (is (= elems (->> gen (realize 1000) second))))))))

(deftest used-initial-gen
  (let [stepped (-> (gen/from-seq (range 100)) step step)
        gens [(gen/map stepped (partial + 100))
              (gen/cycle stepped)
              (gen/mapped {:name (gen/const "qwe")
                           :age stepped})
              (gen/filter stepped even?)
              (gen/random stepped {:block-size 10})
              (gen/weighted [[3 stepped] [1 (gen/const 22)]])]]
    (testing "initializing with used generator"
      (doseq [gen gens]
        (let [[new-gen elems] (realize 5 gen)]
          (is (not= elems (->> new-gen reset (realize 5) second))))))))
