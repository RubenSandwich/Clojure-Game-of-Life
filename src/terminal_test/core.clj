
(ns terminal-test.core
  (:require
    [lanterna.screen :as s]
    [clojure.string :as str]
    [clojure.data :as data]
    ))

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
  (if (state :paused) {} {:timeout 250}))

(defn make-board-index [x y]
  (str x "," y))

(defn build-inital-board [state]
  (let
    [
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
      (repeat false))
    ))

(defn parse-int [s]
  (Integer. s))

(defn check-neighbor [board cell direction]
 ; Without wrapping the edges are considered 'dead'
 (let
   [
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

(defn alive-neighbors [board cell live-count count]
 (if (not= count 8)
   (let
     [
       direction (case count
         0 :left
         1 :top-left
         2 :top
         3 :top-right
         4 :right
         5 :bottom-right
         6 :bottom
         7 :bottom-left)
     ]
     (alive-neighbors
       board
       cell
       (if (check-neighbor board cell direction) (inc live-count) live-count)
       (inc count))
   )
   live-count
 ))

(defn alive-next-generation [board cell]
 (let
   [
     alive (true? (board cell))
     alive-neighbors-count (alive-neighbors board cell 0 0)
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
     )
   )))


; Inital State
(def inital-state {
  :generation 0
  :board {}
  :board-size {:cols 0 :rows 0}
  :cursor {:x 0 :y 0}
  :history []
  :paused true})

(def live-cell-char "▓")


; State modification
(defn next-generation-board [board]
  (reduce-kv
    (fn [m k _]
      (assoc m k (alive-next-generation board k)))
    {} board))

(defn test-one []
  (let
    [
    ; Block Still Life Test
    ; OXX -> OXX
    ; OXX -> OXX
    ; OOO -> OOO
      board {
        "0,0" false
        "1,0" true
        "2,0" true
        "0,1" false
        "1,1" true
        "2,1" true
        "0,2" false
        "1,2" false
        "2,2" false
      }
      expected-board {
        "0,0" false
        "1,0" true
        "2,0" true
        "0,1" false
        "1,1" true
        "2,1" true
        "0,2" false
        "1,2" false
        "2,2" false
      }
      diffed (data/diff (next-generation-board board) expected-board)
    ]
      (if (nil? (first diffed))
        "Block Still Life Test: Passed"
        (str "board: " (first diffed) " next-board: " (second diffed))
      )))

(defn test-two []
  (let
    [
    ; Blinker Oscillators Test
    ; OXO -> OOO
    ; OXO -> XXX
    ; OXO -> OOO
      board {
        "0,0" false
        "1,0" true
        "2,0" false
        "0,1" false
        "1,1" true
        "2,1" false
        "0,2" false
        "1,2" true
        "2,2" false
      }
      expected-board {
        "0,0" false
        "1,0" false
        "2,0" false
        "0,1" true
        "1,1" true
        "2,1" true
        "0,2" false
        "1,2" false
        "2,2" false
      }
      diffed (data/diff (next-generation-board board) expected-board)
    ]
      (if (nil? (first diffed))
        "Blinker Oscillators Test: Passed"
        (str "board: " (first diffed) " next-board: " (second diffed))
      )))

(defn next-generation [state]
  (assoc state :generation (inc (state :generation))))

(defn move-cursor [state direction]
  (assoc state :cursor
    (let [x ((state :cursor) :x) y ((state :cursor) :y)]
      (case direction
        :right {:y y :x (if (= x (last-col state)) x (inc x))}
        :left {:y y :x (if (= x 0) x (dec x))}
        :down {:y (if (= y (last-row-no-ui state)) y (inc y)) :x x}
        :up {:y (if (= y 0) y (dec y)) :x x}
        (state :cursor)
      ))))

(defn pause [state]
  (assoc state :paused true))

(defn toggle-pause [state]
  (assoc state :paused (not (state :paused))))

(defn update-board-size [screen state]
  (assoc state :board-size
    {:cols (screen-width screen) :rows (screen-height screen)}))

(defn toggle-cell [state]
  (assoc state :board
    (let
      [
        x ((state :cursor) :x)
        y ((state :cursor) :y)
        board-index (make-board-index x y)
        cell ((state :board) board-index)
      ]
      (merge (state :board) {board-index (not cell)})
      )))


; Drawing
(defn draw-board [screen state]
  (doseq
    [
      live-cell
      (filter #(second %) (state :board))
    ]
    (let
      [
        loc (first live-cell)
        loc-vec (str/split loc #",")
        x (parse-int (first loc-vec))
        y (parse-int (second loc-vec))
      ]
        (s/put-string screen x y live-cell-char)
      )
    )
)

(defn draw-ui
  [screen state]
  (s/put-string screen 0 (last-row state)
    (str
      (if (state :paused)
        "Paused: 'p' to play"
        "Playing: 'p' to pause")
      " - Generation: " (state :generation)
      " - Controls: 'arrow keys' + 'enter'")
  ))

(defn draw-loop [screen state]
  (s/clear screen)

  ; Tests
  ; (s/put-string screen 0 1
  ;   (str (test-one)))
  ; (s/put-string screen 0 2
  ;   (str (test-two)))

  (s/move-cursor screen (cursor-to-vector state))
  (draw-board screen state)
  (draw-ui screen state)

  (s/redraw screen)
  (let [key (s/get-key-blocking screen (timeout state))]
    (case key
      (:up :down :left :right) (draw-loop screen (move-cursor state key))
      \space (draw-loop screen (toggle-cell state))
      \p (draw-loop screen (toggle-pause state))
      \q () ; Stops the recursion and therefore the app
      (draw-loop screen
        (assoc
          (next-generation state)
          :board
          (next-generation-board (state :board)))
      )
    ))
  )

; Init
(defn main []
  (let [screen (s/get-screen :unix)]
    (s/in-screen screen
      (draw-loop screen
        (let [state (update-board-size screen inital-state)]
          (assoc state :board (build-inital-board state)))
      ))))

(defn -main [& args]
    (main))
