(ns saegeblatt.core
  (:use saegeblatt.characters saegeblatt.world saegeblatt.net))

(def state (atom {:traversaltime 500
                  :direction :left-to-right
                  :starttime nil
                  :current-view []}))

(declare view)
(defn traversal-time-loop
  ([rcvr]
    (let [byte1 (rcvr)
          start (System/currentTimeMillis)]
      (traversal-time-loop rcvr start)))
  ([rcvr start]
    (let [byte2 (rcvr)
          end (System/currentTimeMillis)
          tt (- end start)]
      (reset! state {:traversaltime tt
                     :direction (if (= byte2 0)
                                  :left-to-right
                                  :right-to-left)
                     :starttime end
                     :current-view @view})
      (recur rcvr end))))

(declare conf)
(defmulti calculate-view-index (fn [mode & _] mode))

(defmethod calculate-view-index :linear
  [_ time-since-0 traversaltime direction]
  (let [step (max 1 (/ traversaltime 128))
        steps-done (/ time-since-0 step)]
    (int (mod (+ 64 steps-done) 128))))

(defn direction-key-to-int [k]
  (cond
    (= k :left-to-right) 1
    :else -1))

(defmethod calculate-view-index :bilinear
  [_ time-since-0 traversaltime direction]
  (let [step (max 1 (/ traversaltime 128))
        steps-done (/ time-since-0 step)
        direction-int (direction-key-to-int direction)
        direction-modificator (if (< steps-done 64)
                                direction-int
                                (* -1 direction-int))]
    (int (mod (+ 64 (* direction-modificator steps-done)) 128))))

(defmethod calculate-view-index :harmonic
  [_ time-since-0 traversaltime direction]
  (let [offset (* (Math/sin (/ (* (Math/PI) time-since-0) traversaltime)) 64)]
    (int (+ 64 (* (direction-key-to-int direction) offset)))))

;(defn calculate-view-index [time-since-0 traversaltime direction]
;  (let [step (max 1 (/ traversaltime 128))
;        steps-done (/ time-since-0 step)]
;    (int (mod (+ 64 steps-done) 128))))

(defn show-text [{:keys [starttime traversaltime direction current-view]} 
                 view
                 send-fn]
  (let [view-index (calculate-view-index (:mode @conf) (- (System/currentTimeMillis) starttime) traversaltime direction)]
    (when (and (= 0 view-index))
      (swap! state assoc :current-view view))
    (send-fn (nth current-view view-index))))

(defn render-loop [sender]
  (when (:starttime @state)
    (show-text @state @view sender))
  ;(Thread/sleep 1)
  (recur sender))

(defn push-loop [pusher]
  (when (:starttime @state)
    (pusher))
  (Thread/sleep (/ (:traversaltime @state) 6))
  (recur pusher))

(defmacro parallel [& body]
  (->> (map (fn [expr] `(.start (Thread. (fn [] ~expr))))
            body)
    (cons 'do)))

(def conf (atom {:mode :bilinear}))

(def text "THANK GOD IT'S FRIDAY    ")

(def world (atom (string-to-bytes text)))

(def view (atom @world))

(defn -main [& args]
  (parallel
    (traversal-time-loop (make-receive 13665))
    (world-loop 500 world view scroller-maker 3 bitmap-scroller)
    (render-loop (make-send "localhost" 13664))
    (push-loop (make-send "localhost" 13666))))
