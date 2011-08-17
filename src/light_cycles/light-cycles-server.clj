(ns light-cycles.light-cycles-server
  (:require [light-cycles.server :as server]
	    [light-cycles.client :as client]))

(defn info-handler [connection state]
  (network/send connection {:game :light-cycles}))

(defn connection-handler [connection state]
  (condp = nil
      (:player-1 state) {:player-1 {:position [100 100]}}
      (:player-2 state) {:player-2 {:position [900 900]}}
      (:player-3 state) {:player-3 {:position [900 100]}}
      (:player-4 state) {:player-4 {:position [100 900]}}
      nil))

(server/start {:port 4000
	       :object-codec {}
	       :info-handler info-handler
	       :connection-handler connection-handler})

(def connection (network/connect "localhost" 4000 {}))

(network/send-byte connection server/info-header)
(if (zero? (network/receive-byte connection))
  (println "no connection")
  (println "connection" (network/receive connection)))