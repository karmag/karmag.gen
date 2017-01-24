(ns karmag.gen.document-generator
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [karmag.gen.core :as gen :refer [reset exhausted? elem step]]))

(defmacro ^:private create-sample [code]
  `(let [sample# (loop [gen# ~code, result# [], n# 3]
                   (if (zero? n#)
                     (if (exhausted? gen#)
                       result#
                       (conj result# '...))
                     (if (exhausted? gen#)
                       result#
                       (recur (step gen#)
                              (conj result# (elem gen#))
                              (dec n#)))))]
     (str "    " '~code \newline
          "    ;; " (str "["
                         (->> sample# (interpose ", ") (apply str))
                         "]"))))

(defn- paragraph [text]
  (->> (.split ^String text "\n")
       (map (memfn ^String trim))
       (interpose "\n")
       (apply str)))

(defn- code [& lines]
  (->> (map #(str "    " %) lines)
       (interpose "\n")
       (apply str)))

(def ^:private doc-parts
  ["# Gen"

   "This project uses [semantic versioning](http://semver.org/)."

   "## Generator concept"

   (paragraph "Generators from this library are purely
   functional. This means that they can be treated in the same manner
   as regular clojure data structures and that they will produce
   repeatable results.")

   (paragraph "The primary functions for manipulating a generator are
   `step`, `elem` and `exhausted?` corresponding to *next*, *peek* and
   *has-next* functionality of iterators/generators in
   general. Generators also support a `reset` function that returns
   the \"original\" generator.")

   "## Provided generators"

   (paragraph "This is a brief overview of the generators provided
   through the `karmag.gen.core` namespace. For further information
   see the corresponding function documentation. These will clash with
   definitions in `clojure.core` so they should be required with
   an alias.")

   (code "(require '[karmag.gen.core :as gen"
         "           :refer [step elem exhausted? reset]])")

   "`const` is analogous to `clojure.core/constantly`."
   (create-sample (gen/const 10))

   (paragraph "`from-seq` creates a generator from a sequence or a
   function that creates a sequence.")
   (create-sample (gen/from-seq [1 2 3]))
   (create-sample (gen/from-seq (constantly [:a :b :c])))

   (paragraph "`map` behaves as `clojure.core/map` but the generator
   to operate on is the first argument. All `karmag.gen.core`
   functions take the generator as the first argument.")
   (create-sample (gen/map (gen/const 10) (partial + 8)))

   "`cycle` is analogous to `clojure.core/cycle`."
   (create-sample (gen/cycle (gen/from-seq [1 2])))

   (paragraph "`mapped` takes a {key gen} map and generates elements
   that are {key (elem gen)}.")
   (create-sample (gen/mapped {:a (gen/const 11)
                               :o (gen/const 22)}))

   "`filter` is analogous to `clojure.core/filter`."
   (create-sample (gen/filter (gen/from-seq (range 10)) odd?))

   (paragraph "`random` shuffles the order of another generator. It
   always produces the same result. An additional argument map can be
   given to control the behavior.")
   (create-sample (gen/random (gen/from-seq (range 10))))
   (create-sample (gen/random (gen/from-seq (range 10))
                              {:seed 10}))

   (paragraph "`weighted` consumes generators in order by a given
   weight argument. Generators will be queried the number of times
   given before moving on to the next.")
   (create-sample (gen/weighted [[1 (gen/const :a)]
                                 [10 (gen/const :b)]]))

   ])

(deftest generate-document
  (with-open [writer (io/writer "README.md")]
    (doseq [part doc-parts]
      (.write writer ^String part)
      (.write writer "\n\n"))))
