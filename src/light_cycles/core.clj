(ns light-cycles.server
  (:import java.io.DataInputStream
	   java.io.DataOutputStream
	   java.net.Socket)
  (:use clojure.contrib.server-socket)
  (:require [light-cycles.network :as network]))

(defn start [port]
  (let [connections (atom [])]
    (create-server port (fn [in out]
			  (swap! connections #(conj % out))
			  (let [in (DataInputStream. in)
				out (DataOutputStream. out)]
			    (loop []
			      (dotimes [_ (.readByte in)]
				(condp = (.readByte in)
				    network/keyword-header (do (println "read keyword-header") (network/receive-keyword in out))
				    network/string-header (let [data (byte-array (.readInt in))]
							    (loop [accum 0]
							      (when (< accum (alength data))
								(recur (+ accum (doto (.read in data accum (- (alength data) accum))
										  (println))))))
							    (String. data))
				    network/int-header (do (println "read int-header") (.readInt in))
				    network/float-header (do (println "read float-header") (.readFloat in))))
			      (recur)))))))
(comment (binding [network/codec (atom {})]
	   (start 4000)

	   (def socket (Socket. "localhost" 4000))
	   (def in (DataInputStream. (.getInputStream socket)))
	   (def out (DataOutputStream. (.getOutputStream socket)))
	   (.writeByte out 6)
	   (.writeByte out network/int-header)
	   (.writeInt out 45)
	   (network/send-keyword :foo in out)
	   (.writeByte out network/float-header)
	   (.writeFloat out 34.875)
	   (network/send-keyword :bar in out)
	   (network/send-keyword :foo in out)
	   (network/send-keyword :hello in out)
	   (println @light-cycles.network/codec)))