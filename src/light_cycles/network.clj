(ns light-cycles.network)

(def keyword-header 0)
(def raw-header 1)
(def encoded-header 2)
(def string-header 3)
(def int-header 4)
(def float-header 5)
(def map-header 6)
(def more-header 7)
(def done-header 8)

(def codec)
(def input)
(def output)

					;---------------------------send-------------------------------------------
(def send)
(defn send-int [int]
  (.writeByte output int-header)
  (.writeInt output int))

(defn send-float [float]
  (.writeByte output float-header)
  (.writeFloat output float))

(defn send-string [string]
  (.writeByte output string-header)
  (.writeInt output (count string))
  (.write output (.getBytes string)))

(defn send-keyword [keyword]
  (.writeByte output keyword-header)
  (if-let [code (codec keyword)]
    (do (.writeByte output encoded-header)
	(.writeByte output code)
	codec)
    (let [name (name keyword)
	  length (count name)]
      (.writeByte output raw-header)
      (.writeByte output length)
      (.write output (.getBytes name) 0 length)
      (assoc! codec keyword (/ (count codec) 2) (/ (count codec) 2) keyword))))

(defn send-map [map]
  (.writeByte output map-header)
  (doseq [[k v] map]
    (.writeByte output more-header)
    (send k)
    (send v))
  (.writeByte output done-header))

(defn send [item]
  (cond
   (map? item) (send-map item)
   (keyword? item) (send-keyword item)
   (string? item) (send-string item)
   (integer? item) (send-int item)
   (float? item) (send-float item)))


					;---------------------------receive-------------------------------------------
(def receive)
(defn receive-int []
  (.readInt input))

(defn receive-float []
  (.readFloat input))

(defn receive-string []
  (let [data (byte-array (.readInt input))]
    (loop [accum 0]
      (when (< accum (alength data))
	(recur (+ accum (.read input data accum (- (alength data) accum))))))
    (String. data)))

(defn receive-keyword []
  (if (= encoded-header (.readByte input))
    (codec (.readByte input))
    (let [data (byte-array (.readByte input))
	  _ (loop [accum 0]
	      (when (< accum (alength data))
		(recur (+ accum (.read input data accum (- (alength data) accum))))))
	  keyword (keyword (String. data))]
      (assoc! codec keyword (/ (count codec) 2) (/ (count codec) 2) keyword)
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
      int-header (receive-int)
      float-header (receive-float)))