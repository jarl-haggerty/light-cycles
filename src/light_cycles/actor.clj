(ns light-cycles.actor)

(defmulti update :role)
(defmulti render :role)
(defmulti create :role)
(defmulti process-input :role)