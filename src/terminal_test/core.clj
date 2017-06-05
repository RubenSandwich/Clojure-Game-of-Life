(ns terminal-test.core
  (:require [lanterna.screen :as s]))

; https://sjl.bitbucket.io/clojure-lanterna/reference/#lanternaterminalclear

(def inital-state {
  :generation 0
  :board {}
  :board-size {:cols 0 :rows 0}
  :cursor {:x 0 :y 0}
  :history []
  :paused true})


; (map #(str (if (>= %1 2) (mod %1 2) %1) ":" (mod %1 5)) (range 12))

(defn screen-width [screen]
  (- ((s/get-size screen) 0) 1))

(defn screen-height [screen]
  (- ((s/get-size screen) 1) 1))

(defn last-row [state]
  ((state :board-size) :rows))

(defn last-col [state]
  ((state :board-size) :cols))

(defn cursor-to-vector [state]
  [((state :cursor) :x) ((state :cursor) :y)])


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



(defn next-generation [state]
  (assoc state :generation (+ 1(state :generation))))

(defn move-cursor [state direction]
  (assoc state :cursor
    (let [x ((state :cursor) :x) y ((state :cursor) :y)]
      (case direction
        :right {:y y :x (if (= x (last-col state)) x (+ x 1))}
        :left {:y y :x (if (= x 0) x (- x 1))}
        ; (- (last-row state)) 1) because of the UI
        :down {:y (if (= y (- (last-row state) 1)) y (+ y 1)) :x x}
        :up {:y (if (= y 0) y (- y 1)) :x x}
        (state :cursor)
      ))))

(defn pause [state]
  (assoc state :paused (true)))

(defn toggle-pause [state]
  (assoc state :paused (not (state :paused))))

(defn update-board-size [state screen]
  (assoc state :board-size
    {:cols (screen-width screen) :rows (screen-height screen)}))

(defn timeout [state]
  (if (state :paused) {} {:timeout 750}))

(defn draw-loop [screen state]
  (s/clear screen)
  (s/put-string screen 0 0
    (str
      "Welcome to the Game of Life! "
      (state :cursor)
    ))
  (s/move-cursor screen (cursor-to-vector state))
  (draw-ui screen state)
  (s/redraw screen)
  (let [key (s/get-key-blocking screen (timeout state))]
    (case key
      (:up :down :left :right) (draw-loop screen (move-cursor state key))
      \p (draw-loop screen (toggle-pause state))
      \q (s/stop screen)
      (draw-loop screen (next-generation state))
    ))
  )


(defn main []
  (let [screen (s/get-screen :unix)]
    (s/in-screen screen
      (draw-loop screen (update-board-size inital-state screen)))))

(defn -main [& args]
    (main))
