(ns karmag.gen.performance-test
  (:require [clojure.test :refer :all]
            [karmag.gen.core :as gen]))

(defn- string []
  (-> ["qwe" "asd" "zxc"] gen/from-seq gen/cycle gen/shuffle))

(defn- number []
  (-> (range 1000) gen/from-seq gen/cycle gen/shuffle))

(def ^:private default-keys
  (->> (range (int \a) (inc (int \z)))
       (map char)
       (map str)
       (map keyword)))

(defn- make-gen [max-depth max-kvs]
  (if (zero? max-depth)
    (if (zero? (rand-int 2))
      (string)
      (number))
    (gen/mapped
     (->> (interleave (take max-kvs default-keys)
                      (repeatedly #(make-gen (dec max-depth) max-kvs)))
          (apply hash-map)))))

(defn- run-test [thread-count duration depth kvs]
  (let [g (make-gen depth kvs)
        running (atom true)
        counter (atom 0)
        threads (->> #(Thread. (fn []
                                 (loop [g g]
                                   (when @running
                                     (gen/elem g)
                                     (swap! counter inc)
                                     (recur (gen/step g))))))
                     repeatedly
                     (take thread-count)
                     doall)]
    (doseq [t threads] (.start ^Thread t))
    (let [start-count @counter
          start-time (System/currentTimeMillis)]
      (Thread/sleep duration)
      (let [end-count @counter
            end-time (System/currentTimeMillis)]
        (reset! running false)
        (doseq [t threads] (.join ^Thread t))
        {:duration (- end-time start-time)
         :depth depth
         :kvs kvs
         :count (- end-count start-count)}))))

(deftest performance-test
  (let [duration 1000
        depth-kvs-coll [[1 1] [3 3] [5 5]]
        thread-count 1]
    (println (format "| Performance results, using %d thread(s) for %s ms"
                     thread-count
                     duration))
    (doseq [{:keys [duration depth kvs count]}
            (->> (map (fn [[depth kvs]]
                        (run-test thread-count duration depth kvs))
                      depth-kvs-coll)
                 doall)]
      (let [gens-used (inc (* 3 (reduce * (repeat depth kvs))))
            throughput (float (* 1000 (/ count duration)))]
        (println
         (format "|   Using %-4d gens => %8.0f elem+step / sec [depth=%d kvs=%d]"
                 gens-used
                 throughput
                 depth
                 kvs))))))
