(ns light-cycles.core
  (:require [light-cycles.server :as server]
	    [light-cycles.network :as network]))

(defn info-handler [connection state]
  {:game :light-cycles})

(defn connection-handler [connection state]
  (condp = nil
      (:player-1 state) {:player-1 {:position [100 100]}}
      (:player-2 state) {:player-2 {:position [900 900]}}
      (:player-3 state) {:player-3 {:position [900 100]}}
      (:player-4 state) {:player-4 {:position [100 900]}}
      nil))

(server/create {:port 4000
		:object-codec {}
		:info-handler info-handler
		:connection-handler connection-handler
		:state {}})

(defn -main []
  (let [connection (network/connect "localhost" 4000 {})]
    (network/send connection server/info-header)
    (if (zero? (network/receive connection))
      (println "no info")
      (println "info" (network/receive connection))))

  (let [connection (network/connect "localhost" 4000 {})]
    (network/send connection server/connect-header)
    (if (zero? (network/receive connection))
      (println "no connection")
      (loop [])
      (println "connection" (do (network/send connection server/request-header)
				(network/receive connection))))))