(ns terminal-test.core-test
  (:require [clojure.test :refer :all]
            [terminal-test.core :refer :all]
            [clojure.data :as data]))

(defn test-next-generation [board expected-board]
   (let [diffed (data/diff (next-generation-board board) expected-board)]
     (nil? (first diffed))))

(deftest blinker-oscillators-test
  (testing "Do oscillators work?"
    (let [
      ; Blinker Oscillators
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
      ]
      (is (test-next-generation board expected-board)))))

(deftest block-still-life-test
  (testing "Do still lifes work?"
    (let [
      ; Block Still Life
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
      ]
      (is (test-next-generation board expected-board)))))
