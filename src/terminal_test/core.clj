
(ns terminal-test.core
  (:require
    [lanterna.screen :as s]
    [clojure.string :as str]
    [clojure.data :as data]))

; TODO
; 1. Use a loop instead of a recursion for the draw loop?
; DONE - 2. Implement a popultation count
; 3. Implement wrapping
; 4. When not wrapping the board should be extended over the edges by 2,
; so the board seems infinite
; 5. Create a help screen
; DONE - 6. Allow slowing down and speeding up the timeout
; 6a. Show in FPS instead of ms?
; DONE - 7. Write tests
; 8. Listen for resize events and adjust board size
; DONE = 9. Pause when moving around the board

; https://sjl.bitbucket.io/clojure-lanterna/reference/#lanternaterminalclear

; State/Screen utilities
(defn screen-width [screen]
  (dec (first (s/get-size screen))))

(defn screen-height [screen]
  (dec (second (s/get-size screen))))

(defn last-row [state]
  ((state :board-size) :rows))

(defn last-row-no-ui [state]
  (dec ((state :board-size) :rows)))

(defn last-col [state]
  ((state :board-size) :cols))

(defn cursor-to-vector [state]
  [((state :cursor) :x) ((state :cursor) :y)])

(defn timeout [state]
  (if (state :paused) {} {:timeout (state :generation-time)}))

(defn make-board-index [x y]
  (str x "," y))

(defn build-inital-board [state]
  (let [
      cols (last-col state)
      rows (last-row-no-ui state)
      area (* rows cols)
    ]
    (zipmap
      (map
        #(make-board-index
          (if (>= %1 cols) (mod %1 cols) %1)
          (mod %1 rows))
        (range area))
      (repeat false))))

(defn parse-int [s]
  (Integer. s))

(def directions [
  :left
  :top-left
  :top
  :top-right
  :right
  :bottom-right
  :bottom
  :bottom-left])

(defn check-neighbor [board cell direction]
 ; Without wrapping the edges are considered 'dead'
 (let [
     cell-vec (str/split cell #",")
     x (parse-int (first cell-vec))
     y (parse-int (second cell-vec))
     neighbor (case direction
       :left (make-board-index (dec x) y)
       :top-left (make-board-index (dec x) (dec y))
       :top (make-board-index x (dec y))
       :top-right (make-board-index (inc x) (dec y))
       :right (make-board-index (inc x) y)
       :bottom-right (make-board-index (inc x) (inc y))
       :bottom (make-board-index x (inc y))
       :bottom-left (make-board-index (dec x) (inc y))
     )
   ]
   (true? (board neighbor))))

(defn alive-neighbors [board cell directions live-count]
  (let [direction (peek directions)]
  (if (nil? direction)
    live-count
    (alive-neighbors
      board
      cell
      (pop directions)
      (if (check-neighbor board cell direction)
        (inc live-count)
        live-count)))))

(defn alive-next-generation [board cell]
 (let
   [
     alive (true? (board cell))
     alive-neighbors-count
      (alive-neighbors board cell directions 0)
   ]
   (if alive
     (case alive-neighbors-count
       (0 1) false ; 1. Underpopulation
       (2 3) true ; 2. Stay alive
       false ; 3. Overpopulation
     )
     (case alive-neighbors-count
       3 true ; 4. Reproduction
       false ; 5. Stay dead
     ))))


; Inital State
(def inital-state {
  :generation 0
  :population 0
  :board {}
  :board-size {:cols 0 :rows 0}
  :cursor {:x 0 :y 0}
  :history []
  :paused true
  :generation-time 250})

(def live-cell-char "▓")


; State modification
(defn calculate-population [board]
  (reduce-kv
    (fn [s _ v]
      (if (true? v) (inc s) s))
    0 board))

(defn next-generation-board [board]
  (reduce-kv
    (fn [m k _]
      (assoc m k (alive-next-generation board k)))
    {} board))

(defn pause [state]
  (assoc state :paused true))

(defn update-history [newState state]
  (let [diffed (data/diff (state :board) (newState :board))]
    (assoc newState :history (conj (state :history) (first diffed)))))

(defn next-generation [state]
  (let [newBoard (next-generation-board (state :board))]
  (-> state
    (assoc :generation (inc (state :generation)))
    (assoc :board newBoard)
    (assoc :population (calculate-population newBoard))
    (update-history state))))

(defn prev-generation [state]
  (if (zero? (state :generation))
    state
    (let [newBoard (merge (state :board) (peek (state :history)))]
      (-> state
        (assoc :generation (dec (state :generation)))
        (assoc :board newBoard)
        (assoc :population (calculate-population newBoard))
        (assoc :history (pop (state :history)))
        (pause)))))

(defn move-cursor [state direction]
  (-> state
    (assoc :cursor
      (let [x ((state :cursor) :x) y ((state :cursor) :y)]
        (case direction
          :right {:y y :x (if (= x (last-col state)) x (inc x))}
          :left {:y y :x (if (= x 0) x (dec x))}
          :down {:y (if (= y (last-row-no-ui state)) y (inc y)) :x x}
          :up {:y (if (= y 0) y (dec y)) :x x}
          (state :cursor))))
    (pause)))

(defn toggle-pause [state]
  (assoc state :paused (not (state :paused))))

(defn update-board-size [screen state]
  (assoc state :board-size
    {:cols (screen-width screen) :rows (screen-height screen)}))

(def generation-time-change-interval 50)
(def max-generation-time-change-interval 1000)

(defn update-generation-time [state direction]
  (let [
    new-generation-time
    (if (or
          (and
            (= direction :down)
            (= generation-time-change-interval (state :generation-time)))
          (and
            (= direction :up)
            (= max-generation-time-change-interval (state :generation-time))))
      (state :generation-time)
      (if (= direction :up)
        (+ (state :generation-time) generation-time-change-interval)
        (- (state :generation-time) generation-time-change-interval)
      ))
    ]
    (assoc state :generation-time new-generation-time)))

(defn toggle-cell [state]
  (let [
      x ((state :cursor) :x)
      y ((state :cursor) :y)
      board-index (make-board-index x y)
      cell ((state :board) board-index)
      newCell (not cell)
      population (state :population)
    ]
    (-> state
      (assoc :board (merge (state :board) {board-index newCell}))
      (assoc :population
        (if (true? newCell) (inc population) (dec population)))
      (pause))))


; Drawing
(defn draw-board [screen state]
  (doseq
    [
      live-cell
      (filter #(second %) (state :board))
    ]
    (let [
        loc (first live-cell)
        loc-vec (str/split loc #",")
        x (parse-int (first loc-vec))
        y (parse-int (second loc-vec))
      ]
      (s/put-string screen x y live-cell-char))))

(defn draw-ui
  [screen state]
  (s/put-string screen 0 (last-row state)
    (str
      (if (state :paused)
        "PAUSED"
        "ACTIVE")
      " - Generation: " (state :generation)
      " - Interval: "
        (if (= max-generation-time-change-interval (state :generation-time))
          (str (state :generation-time) "s")
          (str (state :generation-time) "ms")
        )
      " - Population: " (state :population))))

(defn draw-loop [screen state]
  (s/clear screen)
  (s/move-cursor screen (cursor-to-vector state))
  (draw-board screen state)
  (draw-ui screen state)
  (s/redraw screen)
  (let [key (s/get-key-blocking screen (timeout state))]
    (case key
      (:up :down :left :right) (draw-loop screen (move-cursor state key))
      (\= \+) (draw-loop screen (update-generation-time state :up))
      (\- \_) (draw-loop screen (update-generation-time state :down))
      \space (draw-loop screen (toggle-cell state))
      \p (draw-loop screen (toggle-pause state))
      \b (draw-loop screen (prev-generation state))
      \q () ; Stops the recursion and therefore the app
      (draw-loop screen (next-generation state)))))


; Init
(defn main []
  (let [screen (s/get-screen :unix)]
    (s/in-screen screen
      (draw-loop screen
        (let [state (update-board-size screen inital-state)]
          (assoc state :board (build-inital-board state)))))))

(defn -main [& args]
    (main))
