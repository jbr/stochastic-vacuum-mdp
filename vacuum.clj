(ns vacuum
  (:use clojure.contrib.combinatorics))

(def p-dirty 0.05)
(def discount 1)
(def world-rows 2)
(def world-cols 2)
(def world-size [ world-cols world-rows ])
(def epsilon 0.001)

(def actions [:suck :noop :up :down :left :right])

(def all-worlds (vec (map vec (selections [true false] (* world-cols world-rows)))))

(def all-positions (vec (cartesian-product (range 0 world-cols) (range 0 world-rows))))

(def all-states (vec (cartesian-product all-worlds all-positions)))

(def initial-policy-and-values
  (reduce #(assoc %1 %2 [:noop 0]) {} all-states))

(defn- world-pos-to-index [[col row]]
  (+ (* world-cols row) col))

(defn get-in-world [world pos]
  (nth world (world-pos-to-index pos)))

(defn assoc-in-world [world pos val]
  (assoc world (world-pos-to-index pos) val))

(defn get-policy [policy-and-values state]
  (get-in policy-and-values [state 0]))

(defn assoc-policy [policy-and-values state new-policy-for-state]
  (assoc-in policy-and-values [state 0] new-policy-for-state))

(defn get-value [policy-and-values state]
  (get-in policy-and-values [state 1]))

(defn assoc-value [policy-and-values state new-value-for-state]
  (assoc-in policy-and-values [state 1] new-value-for-state))

(defn- determine-dirt-count [world]
  (reduce + (map #(if %1 1 0) world)))

(def dirt-counts (doall (reduce #(assoc %1 %2 (determine-dirt-count %2)) {} all-worlds)))

(defn dirt-count [world] (dirt-counts world))
  
(defn reward [action world]
  (+ (- (dirt-count world))
     (case action
           :suck -1
           (:left :right :up :down) -2
           :noop 0)))

(defn possible-action? [action [col row :as pos]]
  (case action
        :left  (> col 0)
        :up    (> row 0)
        :right (< col (dec world-cols))
        :down  (< row (dec world-rows))
        (:suck :noop) true))

(defn- determine-possible-actions [position]
  (filter #(possible-action? % position) actions))

(def possible-actions
  (doall (reduce #(assoc %1 %2 (determine-possible-actions %2)) {} all-positions)))

(defn take-action [action world [col row :as position]]
  (if (possible-action? action position)
    (case action
          :suck  [(assoc-in-world world position false) position]
          :noop  [world position]
          :left  [world [(dec col) row]]
          :right [world [(inc col) row]]
          :up    [world [col (dec row)]]
          :down  [world [col (inc row)]])
    [world position]))

(defn- determine-transition-probability [from to]
  (loop [p 1
         [from-head & from-rest] from
         [to-head & to-rest] to]

    (let [new-p (* p (case [from-head to-head]
                           [true true]   1
                           [true false]  0
                           [false false] (- 1 p-dirty)
                           [false true]  p-dirty))]

      (if (or (= new-p 0) (empty? from-rest) (empty? to-rest)) new-p
          (recur new-p from-rest to-rest)))))

(def transition-probability-cache
  (doall (reduce #(assoc %1 %2 (apply determine-transition-probability %2)) {} (selections all-worlds 2))))

(defn transition-probability [from to] (transition-probability-cache [from to]))

(defn- determine-next-worlds [world]
  (reduce (fn [worlds possible-world]
            (let [p (transition-probability world possible-world)]
              (if (> p 0)
                (conj worlds [possible-world p])
                worlds)))
          [] all-worlds))


(def next-worlds (doall (reduce #(assoc %1 %2 (determine-next-worlds %2)) {} all-worlds)))

(defn backup [policy-and-values world position action]
  (reduce + (pmap (fn [[next-world p]]
                      (* p (+ (reward action next-world)
                              (* discount
                                 (get-value policy-and-values
                                            [next-world position])))))
                    (next-worlds world))))

(defn updated-value [policy-and-values world position]
  (let [action (get-policy policy-and-values [world position])
        [post-action-world new-position] (take-action action world position)]
    (backup policy-and-values post-action-world new-position action)))

(defn updated-policy [policy-and-values world position]
  (apply max-key
         (fn [action]
           (let [[updated-world new-position] (take-action action world position)]
             (backup policy-and-values updated-world new-position action)))
         (possible-actions position)))

(defn update-values-for-all-states [policy-and-values]
  (reduce (fn [new-policy-and-values [world position :as state]]
            (assoc-value new-policy-and-values state
                          (updated-value policy-and-values world position)))
          policy-and-values
          all-states))

(defn update-policy-for-all-states [policy-and-values]
  (reduce (fn [new-policy-and-values [world position :as state]]
            (assoc-policy new-policy-and-values state
                          (updated-policy policy-and-values world position)))
          policy-and-values
          all-states))

(defn determine-policy [debug]
  (loop [policy-and-values initial-policy-and-values]

    (let [new-policy-and-values (-> policy-and-values
                                    update-policy-for-all-states
                                    update-values-for-all-states
                                    update-values-for-all-states)

          value-diff (apply max (map (fn [state]
                                       (Math/abs (- (get-value new-policy-and-values state)
                                                    (get-value policy-and-values state))))
                                     all-states))]

      (when debug (prn value-diff))
      
      (if (> value-diff epsilon)
        (recur new-policy-and-values)
        new-policy-and-values))))


