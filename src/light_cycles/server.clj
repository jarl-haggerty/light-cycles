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

(def state (atom {}))
(def input (atom {:input []}))

(defn nested-merge [state1 state2]
;  (println 'nested-merge state1 state2)
  (loop [current (merge-with
		  (fn [x y]
;		    (println "    " x y)
		    (if (and (associative? x) (associative? y))
		      (do  (nested-merge x y))
		      (do  y)))
		  state1 state2)
	 [key & more-keys] (keys current)]
    (if key
      (if (= (get current key) drop-element)
	(recur (dissoc current key) more-keys)
	(recur current more-keys))
      current)))

(defn flush-input []
  (:collector (swap! input #(hash-map :input [] :collector (:input %)))))

(defn start-update-loop [state]
  (future
   (doto (Thread/currentThread) (.setName "Server Update Loop"))
   
   (loop [time (System/currentTimeMillis) input (flush-input)]
;     (println 'handle-input input)
;     (println 'updating state)
     (swap! state
	    (fn [{state :state old-delta :delta}]
;	      (println 'hello)
;	      (println 'bye state)
		
	      (let [delta (into {} (for [[k v] state]
					;				     (do (println k v))
				     [k (merge (actor/update v)
					       (actor/process-input v input))]))]
;		(println 'deltas delta)
		{:state (nested-merge state delta)
		 :delta (nested-merge old-delta delta)})))
 ;     (println 'updated)
     (Thread/sleep (max 0 (- 500 (- (System/currentTimeMillis) time))))
  ;     (println 'slept)
     (recur (System/currentTimeMillis) (flush-input)))))

(defn process-input [state input]
  (swap! state
	 (fn [{state :state old-delta :delta}]
					;	      (println 'hello)
					;	      (println 'bye state)
	   (let [delta (into {} (for [[k v] state]
					;				     (do (println k v))
				  [k (actor/process-input v input)]))]
					;		(println 'deltas delta)
	     {:state (nested-merge state delta)
	      :delta (nested-merge old-delta delta)}))))

(defn game-loop [initial-state]
  (Thread/sleep 1000)
  (let [state (atom {:state initial-state
		     :delta initial-state})]
    (start-update-loop state)
    (loop []
;      (println 'listening)
      (condp = (.readByte network/input)
	  request-header (do
;			   (println 'sending)
			   (let [delta  (:collector (swap! state (fn [{state :state delta :delta}]
								   {:state state :delta {}
								    :collector delta})))]
;			     (println 'sending delta)
			     (network/send delta)))
	  input-header (let [new-input (vec (for [_ (range (.readByte network/input))]
					      (network/receive)))]
;			 (println 'new-input new-input)
			 (swap! input #(hash-map :input (into (:input %) new-input)))
					;			 (println 'new-input-after input)
			 ))
      (recur))))

(defn send-info []
  (network/send {:game "Light Cycles" :players 20 :map {:name "Box" :size 23.5}})
  (println 'server (persistent! network/codec)))

(defn start [port initial-state]
  (let [connections (atom [])]
    (create-server port (fn [in out]
			  (binding [network/input (DataInputStream. in)
				    network/output (DataOutputStream. out)
				    network/codec (atom {})]
			    (condp = (.readByte network/input) 
				info-header (send-info)
				connect-header (game-loop initial-state)))))))

(comment (start 4000)
	 (def socket (Socket. "localhost" 4000))
	 (binding [network/input (DataInputStream. (.getInputStream socket))
		   network/output (DataOutputStream. (.getOutputStream socket))
		   network/codec (atom {})]
	   (.writeByte network/output info-header)
	   (println 'hello (network/receive))
	   (println 'client (persistent! network/codec))))