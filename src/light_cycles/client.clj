(ns light-cycles.client
  (:import javax.swing.Timer
	   javax.swing.JFrame
	   java.awt.event.ActionListener
	   java.awt.event.KeyListener
	   java.net.Socket
	   java.io.DataInputStream
	   java.io.DataOutputStream)
  (:require [light-cycles.server :as server]
	    [light-cycles.network :as network]
	    [light-cycles.actor :as actor]
	    [light-cycles.utils :as utils]))

(defn conj-input [input event]
  (swap! input #(assoc % :input (conj (:input %) event))))

(defn start-interface [input input-handler]
  (let [key-listener (proxy [KeyListener] []
		       (keyPressed [event] (conj-input (input-handler {:key-code (.getKeyCode event)
								       :pressed true})))
		       (keyReleased [event] (conj-input (input-handler {:key-code (.getKeyCode event)
									:pressed true})))
		       (keyTyped [event]))
	frame (doto (JFrame. "Light Cycles")
		(.setSize 500 500)
		(.setLocationRelativeTo nil)
		(.addKeyListener key-listener)
		(.setVisible true))]))

(defn start-sync-loop [connection input]
  (future
   (try
     (doto (Thread/currentThread) (.setName "Sync Loop"))
     (loop [time (System/currentTimeMillis)]
       (let [collected-input (utils/flush-input input)]
	 (network/send connection server/input-header)
	 (network/send connection (count input))
	 (doseq [i input]
	   (network/send connection i)))
       (network/send connection server/request-header)
       (let [server-state (network/receive connection)
	     synced-state (nested-merge (:synced @state) server-state)]
	 (swap! state (fn [_] {:synced synced-state
			       :predicted synced-state}))
	 (println server-state synced-state (- 1000 (- (System/currentTimeMillis) time))))
       (Thread/sleep (max 0 (- 1000 (- (System/currentTimeMillis) time))))
       (recur (System/currentTimeMillis)))
     (catch Exception e (println e)))))

(defn start-updater-thread [state]
  (future
   (loop [time (System/currentTimeMillis)]
     (doseq [[_ actor] state]
       (render actor))
     (swap! state
	    (fn [{predicted :predicted :as root}]
	      (let [new-delta (into {} (for [[k v] state]
					 [k (actor/update v)]))]
		(assoc root :state (nested-merge state new-delta)))))
     (Thread/sleep (max 0 (- 500 (- (System/currentTimeMillis) time))))
     (recur (System/currentTimeMillis)))))

(defn connect [address port input-handler object-codec]
  (let [connection (network/connect address port object-codec)
	state (atom {:synced {} :predicted {}})
	input (atom {:input []})]
    (start-sync-loop connection input)
    (start-interface input input-handler)
    (start-updater-thread state)))