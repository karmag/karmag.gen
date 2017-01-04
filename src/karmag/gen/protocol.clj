(ns karmag.gen.protocol)

(defprotocol Generator
  (reset [this])
  (exhausted? [this])
  (elem [this])
  (step [this]))
