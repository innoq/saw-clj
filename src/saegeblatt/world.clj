(ns saegeblatt.world
  (:use saegeblatt.characters))

(defn scroller-maker
  "returns a scroller that scrolls the world across the view by x positions horizontally"
  [offset scroll-fn]
  (let [pos (atom 0)]
    (fn [view world]
      (swap! pos (fn [p] (mod (+ p offset) (count world))))
      (scroll-fn @pos world))))

(defn string-scroller
  [pos world]
  (str (apply str (drop pos world)) (apply str (take pos world))))

(defn bitmap-scroller
  [pos world]
  (vec (flatten (list (drop pos world) (take pos world)))))

(defn world-loop
  "Continuously updates the view with frequency freq"
  [interval world view fn-maker & args]
  (let [evolve (apply fn-maker args)]
    (loop []
      (swap! view evolve @world)
      (Thread/sleep interval)
      (recur))))
