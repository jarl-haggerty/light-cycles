(ns light-cycles.utils)

(defn nested-merge [state1 state2]
  (loop [current (merge-with
		  (fn [x y]
		    (if (and (associative? x) (associative? y))
		      (do  (nested-merge x y))
		      (do  y)))
		  state1 state2)
	 [key & more-keys] (keys current)]
    (if key
      (if (= (get current key) drop-element)
	(recur (dissoc current key) more-keys)
	(recur current more-keys))
      current)))

(defn flush-input [input]
  (:collector (swap! input #(assoc %
			      :input []
			      :collector (:input %)))))