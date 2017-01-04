(ns karmag.gen.qwe
  (:require [karmag.gen.core :as gen]))

(defn- string []
  (gen/random (gen/cycle (gen/from-seq ["qwe" "asd" "zxc"]))))
(defn- number []
  (gen/random (gen/cycle (gen/from-seq (range 10)))))

(def default-keys
  (->> (range (int \a) (inc (int \z)))
       (map char)
       (map str)
       (map keyword)))

(defn make-gen [max-depth max-kvs]
  (if (zero? max-depth)
    (if (zero? (rand-int 2))
      (string)
      (number))
    (gen/mapped
     (->> (interleave (take max-kvs default-keys)
                      (repeatedly #(make-gen (dec max-depth) max-kvs)))
          (apply hash-map)))))
