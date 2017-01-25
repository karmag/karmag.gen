# Gen

This project uses [semantic versioning](http://semver.org/).

## Generator concept

Generators from this library are purely
functional. This means that they can be treated in the same manner
as regular clojure data structures and that they will produce
repeatable results.

The primary functions for manipulating a generator are
`step`, `elem` and `exhausted?` corresponding to *next*, *peek* and
*has-next* functionality of iterators/generators in
general. Generators also support a `reset` function that returns
the "original" generator.

## Provided generators

This is a brief overview of the generators provided
through the `karmag.gen.core` namespace. For further information
see the corresponding function documentation. These will clash with
definitions in `clojure.core` so they should be required with
an alias.

    (require '[karmag.gen.core :as gen
               :refer [step elem exhausted? reset]])

`const` is analogous to `clojure.core/constantly`.

    (gen/const 10)
    ;; [10, 10, 10, ...]

`from-seq` creates a generator from a sequence or a
function that creates a sequence.

    (gen/from-seq [1 2 3])
    ;; [1, 2, 3]

    (gen/from-seq (constantly [:a :b :c]))
    ;; [:a, :b, :c]

`map` behaves as `clojure.core/map` but the generator
to operate on is the first argument. All `karmag.gen.core`
functions take the generator as the first argument.

    (gen/map (gen/const 10) (partial + 8))
    ;; [18, 18, 18, ...]

`cycle` is analogous to `clojure.core/cycle`.

    (gen/cycle (gen/from-seq [1 2]))
    ;; [1, 2, 1, ...]

`mapped` takes a {key gen} map and generates elements
that are {key (elem gen)}.

    (gen/mapped {:a (gen/const 11), :o (gen/const 22)})
    ;; [{:a 11, :o 22}, {:a 11, :o 22}, {:a 11, :o 22}, ...]

`filter` is analogous to `clojure.core/filter`.

    (gen/filter (gen/from-seq (range 10)) odd?)
    ;; [1, 3, 5, ...]

`shuffle` shuffles the order of another generator. It
always produces the same result. An additional argument map can be
given to control the behavior.

    (gen/shuffle (gen/from-seq (range 10)))
    ;; [2, 9, 1, ...]

    (gen/shuffle (gen/from-seq (range 10)) {:seed 10})
    ;; [0, 9, 5, ...]

`weighted` consumes generators in order by a given
weight argument. Generators will be queried the number of times
given before moving on to the next.

    (gen/weighted [[1 (gen/const :a)] [10 (gen/const :b)]])
    ;; [:a, :b, :b, ...]

