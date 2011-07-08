(ns light-cycles.clock
  (:import java.util.Date
	   java.awt.Color)
  (:use light-cycles.actor))

(defrecord Clock [time other]
  Actor
  (update [this]
	  {:time (System/currentTimeMillis)})
  (render [this graphics]
	  (.setColor Color/black)
	  (.drawString graphics 0 0 (-> time Date. .toString))))

(defmethod create :clock [info]
	   (Clock. (System/currentTimeMillis) "hello"))