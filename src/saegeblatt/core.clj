(ns saegeblatt.core
  (:use saegeblatt.characters saegeblatt.world saegeblatt.net))

(def state (atom {:traversaltime 500
                  :direction :left-to-right
                  :starttime nil
                  :current-view @view}))

(def stop (atom nil))

(defn measure-traversal-time
  ([rcvr]
    (let [byte1 (rcvr)
          start (System/currentTimeMillis)]
      (measure-traversal-time rcvr start)))
  ([rcvr start]
    (let [byte2 (rcvr)
          end (System/currentTimeMillis)
          tt (- end start)]
      ;(println tt)
      (reset! state {:traversaltime tt
                     :direction (if (= byte2 0)
                                  :left-to-right
                                  :right-to-left)
                     :starttime end
                     :current-view @view})
      (when (not @stop)
        (recur rcvr end)))))

(def sender (make-send "localhost" 13664))

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

(let [last-idx (atom -1)]
  (defn show-text [{:keys [starttime traversaltime direction current-view]} view]
    (let [view-index (calculate-view-index (:mode @conf) (- (System/currentTimeMillis) starttime) traversaltime direction)]
      (when (and (= 0 view-index) (not= view-index @last-idx))
        (swap! state assoc :current-view view))
      (reset! last-idx view-index)
      (sender (nth current-view view-index)))))

(defn render-loop []
  ;(println @state)
  ;(println @view)
  ;(show-text @state @view)
  (when (:starttime @state)
    (show-text @state @view))
  ;(Thread/sleep 1)
  (recur))

(defn push-loop [pusher]
  (when (:starttime @state)
    (pusher))
  (Thread/sleep (/ (:traversaltime @state) 6))
  (recur pusher))

(defn in-thread [f]
  (.start (Thread. f)))

(defn init-measurement []
  (in-thread #(measure-traversal-time (make-receive 13665))))

(defn init-world []
  (in-thread #(run render-view 500 world view scroller-maker 3 bitmap-scroller)))

(defn init-rendering []
  (in-thread #(render-loop)))

(defn init-pushing [pusher]
  (in-thread #(push-loop pusher)))

(def conf (atom {:mode :harmonic}))

(defn -main [& args]
  (init-measurement)
  (init-world)
  (init-rendering)
  (init-pushing (make-send "localhost" 13666)))


(defmacro in-parallel
  "Evaluates test. If logical true, evaluates body in an implicit do."
  [& body]
  (list 'do body))