(ns vacuum-gui
  (:use vacuum)
  (:import (javax.swing JFrame JComponent))
  (:import (java.awt.event ActionListener))
  (:import (java.awt Rectangle Graphics2D Dimension Color)))

(def square-size 30)
(def offset 100)

(defn repaint [display action world points [current-x current-y :as pos]]
  (update-proxy display
                {"paint" (fn [this g]
                           (let [g2 ^Graphics2D g]
                             (.setColor g2 Color/black)
                             (.drawString g2 (str action " " points)
                                          100 (/ offset 2))

                             (doseq [[col row :as pos] all-positions]
                               (let [rectangle (Rectangle. (+ offset (* square-size col))
                                                           (+ offset (* square-size row))
                                                           square-size square-size)]
                                 (if (get-in-world world pos)
                                   (.fill g2 rectangle)
                                   (.draw g2 rectangle))))

                             (.setColor g2 Color/red)
                             (.fill g2 (Rectangle. (+ offset (* square-size (+ current-x 0.25)))
                                                   (+ offset (* square-size (+ current-y 0.25)))
                                                   (/ square-size 2)
                                                   (/ square-size 2)))))})
  (.repaint display))


(defn stochastic-arrival [world]
  (vec (map #(if (< (rand) p-dirty) true %1) world)))

(defn choose-action [world position policy]
  (policy [world position]))

(defn run-world [display initial-world policy]
  (loop [world initial-world
         pos [0 0]
         points 0]
    
    (Thread/sleep 100)

    (let [action (get-policy policy [world pos])
          [next-world new-pos] (take-action action world pos)
          points (reward action next-world)]

      (repaint display action world points pos)
      
      (recur (stochastic-arrival next-world)
             new-pos
             points))))

(defn init [f]
  (let [frame (JFrame. "Vacuum World")
        vacuum-display (proxy [JComponent] [] (paint [g]))]
    (.. frame getContentPane (add vacuum-display))
    (doto frame
      (.setPreferredSize (Dimension. (+ (* 2 offset) (* square-size (first world-size)))
                                     (+ (* 2 offset) (* square-size (second world-size)))))
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      .pack
      (.setVisible true))
    (f vacuum-display)))

(when (= "run" (last *command-line-args*))
  (init (fn [display]
        (run-world display
                   (vec (map (fn [_] false)
                             (range (* world-rows world-cols))))
                   (determine-policy true)))))

(prn *command-line-args*)
