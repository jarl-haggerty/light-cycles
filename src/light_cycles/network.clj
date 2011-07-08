(ns light-cycles.network)

(def keyword-header 0)
(def raw-header 1)
(def encoded-header 2)
(def string-header 3)
(def long-header 4)
(def double-header 5)
(def map-header 6)
(def more-header 7)
(def done-header 8)

(def codec)
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
  (doseq [[k v] map]
    (.writeByte output more-header)
    (send k)
    (send v))
  (.writeByte output done-header))

(defn send [item]
  (cond
   (associative? item) (send-map item)
   (keyword? item) (send-keyword item)
   (string? item) (send-string item)
   (integer? item) (send-long item)
   (float? item) (send-double item)))


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
  (loop [accum {}]
    (if (= (.readByte input) more-header)
      (let [key (receive)
	    value (receive)]
	(recur (assoc accum key value)))
      accum)))

(defn receive []
  (condp = (.readByte input)
      map-header (receive-map)
      keyword-header (receive-keyword)
      string-header (receive-string)
      long-header (receive-long)
      double-header (receive-double)))