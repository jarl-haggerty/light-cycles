(ns light-cycles.client
  (:import javax.swing.Timer
	   java.awt.event.ActionListener
	   java.net.Socket
	   java.io.DataInputStream
	   java.io.DataOutputStream)
  (:require [light-cycles.server :as server]
	    [light-cycles.network :as network]
	    [light-cycles.actor :as actor]))

(def state (atom {:synced nil :predicted nil}))

(defn nested-merge [state1 state2]
  (into {} (filter #(not= (second %) server/drop)
		   (merge-with
		    (fn [x y]
		      (if (and (associative? x) (associative? y))
			(nested-merge x y)
			y))
		    state1 state2))))

(defn start-sync-loop [input output codec]
  (future
   (doto (Thread/currentThread) (.setName "Sync Loop"))
   (binding [network/input input
	     network/output output
	     network/codec codec]
     (loop [time (System/currentTimeMillis)]
       (.writeByte network/output server/request-header)
       (let [server-state (network/receive)
	     synced-state (nested-merge (:synced @state) server-state)]
	 (println server-state synced-state)
	 (swap! state (fn [_] {:synced synced-state
			       :predicted synced-state})))
       (Thread/sleep (- 1000 (- (System/currentTimeMillis) time)))
       (recur (System/currentTimeMillis))))))

(defn update [state]
  (into {} (for [[key actor] state]
	     [key (merge actor (actor/update actor))])))

(defn start-game-loop []
  (doto (Timer. 16 (proxy [ActionListener] []
		     (actionPerformed [_]
				      (.writeByte network/output server/request-header)
				      (swap! state #(assoc % :predicted (update (:predicted %)))))))
    (.start)))

(defn render [state]
  (doseq [[_ actor] state]
    (render actor)))

(defn start-render-loop []
  (doto (Timer. 16 (proxy [ActionListener] []
		     (actionPerformed [_]
				      (render (:predicted @state)))))
    (.start)))

(defn info [address port]
  (let [socket (Socket. address port)]
    (binding [network/input (DataInputStream. (.getInputStream socket))
	      network/output (DataOutputStream. (.getOutputStream socket))
	      network/codec (atom {})]
      (.writeByte network/output server/info-header)
      (network/receive))))

(defn connect [address port]
  (doto (Thread/currentThread) (.setName "Main"))
  (let [socket (Socket. address port)]
    (println socket)
    (binding [network/input (DataInputStream. (.getInputStream socket))
	      network/output (DataOutputStream. (.getOutputStream socket))]
      (.writeByte network/output server/connect-header)
      (start-sync-loop network/input network/output (atom {})))))