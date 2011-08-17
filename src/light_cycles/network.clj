(ns light-cycles.network
  (:refer-clojure :exclude [send])
  (:import java.util.Date
	   java.net.Socket
	   java.io.DataInputStream
	   java.io.DataOutputStream))

(def keyword-header 0)
(def raw-header 1)
(def encoded-header 2)
(def string-header 3)
(def long-header 4)
(def double-header 5)
(def map-header 6)
(def more-header 7)
(def done-header 8)
(def object-header 9)

(defn write-byte [{output :output} byte]
  (.writeByte output byte))
(defn read-byte [{input :input}]
  (.readByte input))

					;---------------------------send-------------------------------------------
(def send)
(defn send-long [{output :output} long]
  (.writeByte output long-header)
  (.writeLong output long))

(defn send-double [{output :output} double]
  (.writeByte output double-header)
  (.writeDouble output double))

(defn send-string [{output :output} string]
  (.writeByte output string-header)
  (.writeInt output (count string))
  (.write output (.getBytes string)))

(defn send-keyword [{input :input output :output keyword-codec :keyword-codec} keyword]
  (.writeByte output keyword-header)
  (if-let [code (@keyword-codec keyword)]
    (do (.writeByte output encoded-header)
	(.writeByte output code)
	keyword-codec)
    (let [name (name keyword)
	  length (count name)]
      (.writeByte output raw-header)
      (.writeByte output length)
      (.write output (.getBytes name) 0 length)
      (swap! keyword-codec #(assoc % keyword (/ (count %) 2) (/ (count %) 2) keyword)))))

(defn send-map [{output :output :as connection} map]
  (.writeByte output map-header)
  (.writeByte output (count map))
  (doseq [[k v] (if (vector? map)
		  (map-indexed #(vector %1 %2) map)
		  map)]
    (send connection k)
    (send connection v)))

(defn send-object [{output :output object-codec :object-codec :as connection} object]
  (.writeByte output object-header)
  (let [[header & encoder] (object-codec (type object))]
    (.writeByte output header)
    (.writeByte output (count encoder))
    (doseq [e encoder]
      (send connection (e object)))))

(defn send [connection item]
  (cond
   (associative? item) (send-map connection item)
   (keyword? item) (send-keyword connection item)
   (string? item) (send-string connection item)
   (integer? item) (send-long connection item)
   (float? item) (send-double connection item)
   :else (send-object connection item)))


					;---------------------------receive-------------------------------------------
(def receive)
(defn receive-long [{input :input}]
  (.readLong input))

(defn receive-double [{input :input}]
  (.readDouble input))

(defn receive-string [{input :input}]
  (let [data (byte-array (.readInt input))]
    (loop [accum 0]
      (when (< accum (alength data))
	(recur (+ accum (.read input data accum (- (alength data) accum))))))
    (String. data)))

(defn receive-keyword [{input :input output :output keyword-codec :keyword-codec}]
  (if (= encoded-header (.readByte input))
    (let [code (int (.readByte input))]
      (@keyword-codec code))
    (let [data (byte-array (.readByte input))
	  _ (.readFully input data)
	  keyword (keyword (String. data))]
      (swap! keyword-codec #(assoc % keyword (/ (count %) 2) (/ (count %) 2) keyword))
      keyword)))

(defn receive-map [{input :input :as connection}]
  (into {} (for [_ (range (.readByte input))]
	     [(receive connection) (receive connection)])))

(defn receive-object [{object-codec :object-codec input :input :as connection}]
  (let [decoder (object-codec (int (.readByte input)))
	data (doall (for [_ (range (.readByte input))]
		      (receive connection)))]
    (apply decoder data)))

(defn receive [{input :input :as connection}]
  (condp = (.readByte input)
      map-header (receive-map connection)
      keyword-header (receive-keyword connection)
      string-header (receive-string connection)
      long-header (receive-long connection)
      double-header (receive-double connection)
      object-header (receive-object connection)))

(defn connect [address port object-codec]
  (let [socket (Socket. address port)]
    {:input (-> socket
		.getInputStream
		DataInputStream.)
     :output (-> socket
		.getOutputStream
		DataOutputStream.)
     :keyword-codec (atom {})
     :object-codec object-codec}))

(defn connection-from-streams [input output object-codec]
   {:input (DataInputStream. input)
    :output (DataOutputStream. output)
    :keyword-codec (atom {})
    :object-codec object-codec})