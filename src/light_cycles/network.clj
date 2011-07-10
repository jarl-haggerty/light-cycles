(ns light-cycles.network
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

(def codec)
(def object-codec
     {Date [0 #(.getTime %)]
      0 #(Date. %1)})
(def input)
(def output)

					;---------------------------send-------------------------------------------
(def send)
(defn send-long [long]
  (.writeByte output long-header)
  (.writeLong output long))

(defn send-double [double]
  (.writeByte output double-header)
  (.writeDouble output double))

(defn send-string [string]
  (.writeByte output string-header)
  (.writeInt output (count string))
  (.write output (.getBytes string)))

(defn send-keyword [keyword]
  (.writeByte output keyword-header)
  (if-let [code (@codec keyword)]
    (do (.writeByte output encoded-header)
	(.writeByte output code)
	codec)
    (let [name (name keyword)
	  length (count name)]
      (.writeByte output raw-header)
      (.writeByte output length)
      (.write output (.getBytes name) 0 length)
      (swap! codec #(assoc % keyword (/ (count %) 2) (/ (count %) 2) keyword)))))

(defn send-map [map]
  (.writeByte output map-header)
  (.writeByte output (count map))
  (doseq [[k v] map]
    (send k)
    (send v)))

(defn send-object [object]
  (.writeByte output object-header)
  (let [[header & encoder] (object-codec (type object))]
    (.writeByte output header)
    (.writeByte output (count encoder))
    (doseq [e encoder]
      (send (e object)))))

(defn send [item]
  (cond
   (associative? item) (send-map item)
   (keyword? item) (send-keyword item)
   (string? item) (send-string item)
   (integer? item) (send-long item)
   (float? item) (send-double item)
   :else (send-object item)))


					;---------------------------receive-------------------------------------------
(def receive)
(defn receive-long []
  (.readLong input))

(defn receive-double []
  (.readDouble input))

(defn receive-string []
  (let [data (byte-array (.readInt input))]
    (loop [accum 0]
      (when (< accum (alength data))
	(recur (+ accum (.read input data accum (- (alength data) accum))))))
    (String. data)))

(defn receive-keyword []
  (if (= encoded-header (.readByte input))
    (let [code (int (.readByte input))]
      (@codec code))
    (let [data (byte-array (.readByte input))
	  _ (loop [accum 0]
	      (when (< accum (alength data))
		(recur (+ accum (.read input data accum (- (alength data) accum))))))
	  keyword (keyword (String. data))]
      (swap! codec #(assoc % keyword (/ (count %) 2) (/ (count %) 2) keyword))
      keyword)))

(defn receive-map []
  (into {} (for [_ (range (.readByte input))]
	     [(receive) (receive)])))

(defn receive-object []
  (let [decoder (object-codec (int (.readByte input)))
	data (doall (for [_ (range (.readByte input))]
		      (receive)))]
    (apply decoder data)))

(defn receive []
  (condp = (.readByte input)
      map-header (receive-map)
      keyword-header (receive-keyword)
      string-header (receive-string)
      long-header (receive-long)
      double-header (receive-double)
      object-header (receive-object)))

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