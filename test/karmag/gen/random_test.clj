(ns karmag.gen.random-test
  (:require [clojure.test :refer :all]
            [karmag.gen.random :as r]))

(defn- iterate-with [rng f]
  (let [[value rng] (f rng)]
    (lazy-cat [value] (iterate-with rng f))))

(deftest integerrandom-generation
  (is (= [2147482884 2092764894 1390461064 715295839 79337801]
         (take 5 (iterate-with (r/create 0) r/next-int))))
  (is (= [9223368757592526558 5971984796956658783
          340753260978829684 6130268162485555074 1882825882384093239]
         (take 5 (iterate-with (r/create 0) r/next-long)))))

(deftest uniqueness-test
  (is (= 99998 (->> (iterate-with (r/create 0) r/next-int)
                    (take 100000)
                    set
                    count)))
  (is (= 100000 (->> (iterate-with (r/create 0) r/next-long)
                     (take 100000)
                     set
                     count))))
