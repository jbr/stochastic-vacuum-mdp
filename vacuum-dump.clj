(ns vacuum-dump
  (:use vacuum))


(defn lazy-csv [ & items]
  (apply str (interpose ", " items)))

(defn world-as-string [world]
  (apply str (map #(if % "t" "f") world)))

(doseq [[[world [col row]] [policy value]] (determine-policy true)]
  (println (lazy-csv p-dirty
                     discount epsilon
                     world-cols world-rows
                     (world-as-string world)
                     col row
                     policy
                     value)))

(System/exit 0)
