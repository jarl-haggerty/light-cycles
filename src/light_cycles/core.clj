(ns light-cycles.server
  (:import java.io.DataInputStream
	   java.io.DataOutputStream
	   java.net.Socket
	   javax.swing.Timer
	   java.awt.event.ActionListener)
  (:use clojure.contrib.server-socket)
  (:require [light-cycles.network :as network]))

(def info-header 0)
(def connect-header 1)
(def request-header 2)
(def input-header 2)

(def state (atom {}))

(def running (atom true))

(def timer (doto (Timer. 1000
			 (proxy [ActionListener] []
			   (actionPerformed [_]
					    (swap! running not))))
	     (.setRepeats false)
	     (.start)))

(defn game-loop []
  (loop []
    (condp = (.readByte network/input)
	request-header ()
	input-header ())
    (if @running
      (recur))))

(defn send-info []
  (network/send {:game "Light Cycles" :player 20})
  (println 'server (persistent! network/codec)))

(defn start [port]
  (let [connections (atom [])]
    (create-server port (fn [in out]
			  (binding [network/input (DataInputStream. in)
				    network/output (DataOutputStream. out)
				    network/codec (transient {})]
			    (condp = (.readByte network/input) 
				info-header (send-info)
				connect-header (game-loop)))))))

(start 4000)
(def socket (Socket. "localhost" 4000))
(binding [network/input (DataInputStream. (.getInputStream socket))
	  network/output (DataOutputStream. (.getOutputStream socket))
	  network/codec (transient {})]
  (.writeByte network/output info-header)
  (println 'hello (network/receive))
  (println 'client (persistent! network/codec)))