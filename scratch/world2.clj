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

(defn render-view
  [view]
  (println @view))

(def w (vec (range 0 10)))

(defn f
  [w idx]
  ())

(defn world-seq
  "Returns a lazy seq of all the world statuses, transformed by fn with initial args"
  [world fn & args]
  (lazy-seq
   (cons
    (let [[world & new-args] (apply fn world args)]
      ((world-seq world fn new-args))))))


(defn rotate
  [v]
  (conj (vec (rest v)) (first v)))

;(defn bitmap-vert-scroller
;   [pos world]
;  (vec (map rotate world)))



(def text "THY ")
(def world (string-to-bytes text))
(def view (atom []))


(add-watch view :default (fn [_ _ _ new] (println (subvec n 3 11))))
(run 50 world view scroller-maker 1 bitmap-scroller)