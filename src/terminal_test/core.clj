(ns terminal-test.core
  (:require
    [lanterna.screen :as s]
    [clojure.string :as str]
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
  (if (state :paused) {} {:timeout 750}))

(defn build-inital-board [state]
  (let
    [
      cols (last-col state)
      rows (last-row-no-ui state)
      area (* rows cols)
    ]
    (zipmap
      (map
        #(str (if (>= %1 cols) (mod %1 cols) %1) "," (mod %1 rows))
        (range area))
      (repeat false))
    ))

(defn parse-int [s]
   (Integer. s))


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
        pos (str x "," y)
        cell ((state :board) pos)
      ]
      (merge (state :board) {pos (not cell)})
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
  (s/put-string screen 0 0
    (str
      "Welcome to the Game of Life! "
    ))
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
      (draw-loop screen (next-generation state))
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
