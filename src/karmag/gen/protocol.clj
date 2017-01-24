(ns karmag.gen.protocol)

(defprotocol Generator
  (reset [this]
    "Resets this generator. The exact meaning of this may vary but it
    is generally expected that reset is called on any wrapped
    generators. This means that reset should return a generator that
    is equivalent to this generator as it was created.")
  (exhausted? [this]
    "Returns logical truth if elem can be called on this generator.")
  (elem [this]
    "Returns the current element. This must return the same element
    for multiple invokations.")
  (step [this]
    "Returns a generator that has been stepped once. The exact meaning
    of a step is generator specific."))
