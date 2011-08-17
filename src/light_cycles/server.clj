(ns light-cycles.server
  (:import java.io.DataInputStream
	   java.io.DataOutputStream
	   java.net.Socket
	   javax.swing.Timer
	   java.awt.event.ActionListener)
  (:use clojure.contrib.server-socket)
  (:require [light-cycles.network :as network]
	    [light-cycles.actor :as actor]))

(def info-header 0)
(def connect-header 1)
(def request-header 2)
(def input-header 3)
(def drop-element ::drop)

(defn flush-input [input]
  (:collector (swap! input #(assoc %
			      :input []
			      :collector (:input %)))))

(defn start-updater-thread [state input]
  (future
   (loop [time (System/currentTimeMillis) collected-input (flush-input input)]
     (swap! state
	    (fn [{state :state deltas :deltas :as root}]
	      (let [new-delta (into {} (for [[k v] state]
					 [k (nested-merge (actor/update v)
							  (actor/process-input v collected-input))]))]
		(-> root
		    (assoc :state (nested-merge state new-delta))
		    (assoc :deltas (into deltas (for [[connection delta] deltas]
						  [connection (nested-merge delta new-delta)])))))))
     (Thread/sleep (max 0 (- 500 (- (System/currentTimeMillis) time))))
     (recur (System/currentTimeMillis) (flush-input input)))))

(defn start-network-thread [connection state input]
  (loop []
      (condp = (network/receive connection)
	  request-header (do
			   (let [delta (get-in (swap! state #(-> %
								 (assoc-in [:collector connection] (get-in % [:deltas connection]))
								 (assoc-in [:deltas connection] nil)))
					       [:collector connection])]
			     (println 'delta delta)
			     (network/send connection delta)))
	  input-header (let [new-input (vec (for [_ (range (network/read-byte connection))]
					      (network/receive connection)))]
			 (swap! input #(hash-map :input (into (:input %) new-input)))))
      (recur)))

(defn create [{port :port
	       object-codec :object-codec
	       info-handler :info-handler
	       connection-handler :connection-handler
	       state :state}]
  (let [state (atom {:state state :deltas {}})
	input (atom {:input []})]
    (start-updater-thread state input)
    (create-server port (fn [input output]
			  (let [connection (network/connection-from-streams input output object-codec)]
			    (condp = (network/receive connection)
				info-header (if-let [info (info-handler connection state)]
					      (do (network/send connection 1)
						  (network/send connection info))
					      (network/send connection 0))
				connect-header (if-let [delta (connection-handler connection state)]
						 (do (network/send connection 1)
						     (swap! state (fn [{state :state deltas :deltas :as root}]
								    (let [new-state (nested-merge state delta)]
								      (-> root
									  (assoc :state new-state)
									  (assoc-in [:deltas connection] new-state)))))
						     (start-network-thread connection state input))
						 (network/send connection 0))))))))