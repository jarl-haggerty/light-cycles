(ns light-cycles.network)

(def keyword-header 0)
(def raw-header 1)
(def encoded-header 2)
(def string-header 3)
(def int-header 4)
(def float-header 5)

(def codec (atom {}))

(defn send-keyword [keyword input output]
  (.writeByte output keyword-header)
  (comment (println "sent keyword-header"))
  (if-let [code (@codec keyword)]
    (do (.writeByte output encoded-header)
	(.writeByte output code))
    (let [name (name keyword)
	  length (count name)]
      (.writeByte output raw-header)
      (comment (println "sent raw-header"))
      (.writeByte output length)
      (comment (println "sent length"))
      (.write output (.getBytes name) 0 length)
      (println "sent keyword name" (seq (.getBytes name)))
      (let [code (.readByte input)]
	(comment (println "read code"))
	(swap! codec #(assoc % keyword code code keyword))))))

(defn receive-keyword [input output]
  (if (= raw-header (.readByte input))
    (do    (println "read raw-header")
	   (let [data (byte-array (.readByte input))
		 _ (println "read keyword length" (alength data))
		 _ (loop [accum 0]
		     (println "reading" (String. data) accum (alength data))
		     (when (< accum (alength data))
		       (recur (+ accum (doto (.read input data accum (- (alength data) accum))
					 (println))))))
		 _ (println "read keyword name" (String. data) (seq data))
		 keyword (keyword (String. data))]
	     (if-let [code (@codec keyword)]
	       (.writeByte output code)
	       (do (.writeByte output ((swap! codec #(assoc % keyword (/ (count %) 2) (/ (count %) 2) keyword)) keyword))
		   (println "sent code")))))
    (@codec (.readByte input))))

(defn send-string [string output]
  (.writeByte output string-header)
  (.writeInt output (count string))
  (.write output (.getBytes string)))

(defn send-int [int output]
  (.writeByte output int-header)
  (.writeInt output int))

(defn send-float [float output]
  (.writeByte output float-header)
  (.writeFloat output float))

(defn send [output input & transmission]
  (.writeByte output input (count transmission))
  (doseq [piece transmission]
    (cond
     (keyword? piece) (send-keyword piece input output)
     (string? piece) (send-string piece output)
     (integer? piece) (send-int piece output)
     (float? piece) (send-float piece output))))