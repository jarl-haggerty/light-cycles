(ns light-cycles.core
  (:import java.net.BindException)
  (:use light-cycles.actor
	light-cycles.clock)
  (:require [light-cycles.server :as server]
	    [light-cycles.client :as client]))

(try
  (server/start 4000 {:clock (create {:role :clock})})
  (println "created server")
  (catch BindException _
    (println "server already created")))

(client/connect "localhost" 4000)