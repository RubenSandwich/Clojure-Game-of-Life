(ns terminal-test.core
  (:require [lanterna.screen :as s]))


(def inital-state {
  :generation 0
  :history []
  :paused true})


(defn screen-width
  [screen]
  ((s/get-size screen) 0))


(defn screen-height
  [screen]
  ((s/get-size screen) 1))


(defn last-line
  [screen]
  (- (screen-height screen) 1))


(defn draw-ui
  [screen state]
  (s/put-string screen 0 (last-line screen)
    (str
      (if (state :paused)
        "Paused: 'p' to play"
        "Playing: 'p' to pause")
      " - Generation: " (state :generation)
      " - Controls: 'arrow keys' + 'enter'")
  ))


(defn next-generation [state]
  (assoc state :generation (+ 1(state :generation))))

(defn toggle-pause [state]
  (assoc state :paused (not (state :paused))))

(defn timeout [state]
  (if (state :paused) {} {:timeout 750}))

(defn draw-loop [screen state]
  (s/clear screen)
  (s/put-string screen 0 0 "Welcome to the Game of Life!")
  (draw-ui screen state)
  (s/redraw screen)
  (condp = (s/get-key-blocking screen (timeout state))
    \q "Quiting"
    \p (draw-loop screen (toggle-pause state))
    (draw-loop screen (next-generation state))
  ))


(defn main []
  (let [screen (s/get-screen :unix)]
    (s/in-screen screen
      (draw-loop screen inital-state))))

(defn -main [& args]
    (main))
