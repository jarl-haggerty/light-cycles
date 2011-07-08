(ns light-cycles.actor)

(defprotocol Actor
  (update [this])
  (render [this graphics]))

(defmulti create :role)