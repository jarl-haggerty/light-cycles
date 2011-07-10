(ns light-cycles.clock
  (:import java.util.Date
	   java.awt.Color)
  (:use light-cycles.actor))

(defmethod update :clock [{initial-time :initial-time}]
	   {:time (Date. (- (System/currentTimeMillis) initial-time))})
(defmethod process-input :clock [_ input]
	   (into {} (for [i input]
		      (condp = i
			  :reset [:initial-time (System/currentTimeMillis)]))))
(defmethod render :clock [{time :time} graphics]
  (.setColor Color/black)
  (.drawString graphics 0 0 (.toString time)))
(defmethod create :clock [info]
	   {:role :clock :time (Date. (long 0)) :initial-time (System/currentTimeMillis) :other "hello"})
