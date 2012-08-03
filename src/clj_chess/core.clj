(ns clj-chess.core
    (:use clojure.repl)
    (:require [clojure.string :as string]))

;;; utilties

(defn is-upper [char] (= (string/upper-case char) char))

(def WHITE)
(def BLACK)
(def ROOK \r)
(def KNIGHT \n)
(def BISHOP \b)
(def QUEEN \q)
(def KING \k)
(def PAWN \p)
(def EMPTY \space)
(def INVALID :INVALID)
(def STRAIGHT-DIRS [[1 0] [-1 0] [0 1] [0 -1]])
(def DIAGONAL-DIRS [[1 1] [1 -1] [-1 -1] [-1 1]])
(def ALL-DIRS (concat STRAIGHT-DIRS DIAGONAL-DIRS))

(defn color-of [piece] (if (is-upper (str piece)) WHITE BLACK))
(defn kind-of [piece]  (nth (string/lower-case piece) 0))

; convert a board (in some known format) to the representation used
; throughout the program.
(defn to-board [board]
     (cond (and (vector? board) (string? (board 0)))
           (vec (map vec board))))

(defn to-picture [board]
  (string/join "\n" (map (partial apply str) board)))

(defn print-board [board] (println (to-picture board)))

(defn board-with-moves [board moves]
  (reduce (partial with-piece \x) board moves))

(def initial-board
     (to-board 
       ["rnbqkbnr"
        "pppppppp"
        "        "
        "        "
        "        "
        "        "
        "PPPPPPPP"
        "RNBQKBNR"]))

(defn with-piece [piece board pos]
  (assoc-in board pos piece))

(defn piece-at [board pos]
  (get-in board pos))

(defn is-empty [board pos] (= (piece-at board pos) EMPTY))

; returns color in mate, or nil
(defn is-mate [board] nil)

(defn add-pair [p0 p1]
  [(+ (p0 0) (p1 0))
   (+ (p0 1) (p1 1))])

(defn moves-in-dirs [board pos color dirs dist]
  (apply concat
         (for [dir dirs]
              (loop [cur-pos (add-pair pos dir)
                     moves []]
                    (let [piece-there (piece-at board cur-pos)
                          new-moves (conj moves cur-pos)] 
                      (cond (= piece-there INVALID)
                            moves
                            (not= piece-there BLANK)
                            (if (= color (color-of piece-there))
                              moves
                              new-moves)
                            :else 
                            (recur (add-pair cur-pos dir) new-moves)))))))

;; assumes board is oriented such that current player's home rank is 7
(defn valid-moves [board pos]
  (let [piece (piece-at board pos)
        color (color-of piece)
        kind (kind-of piece)]
    (cond (= kind ROOK)
            (moves-in-dirs board pos color STRAIGHT-DIRS 8)
          (= kind BISHOP)
            (moves-in-dirs board pos color DIAGONAL-DIRS 8)
          (= kind QUEEN)
            (moves-in-dirs board pos color ALL-DIRS 8))))

;; def pawn_moves():
;;   move = [-1 0]
;;   if is_valid(move) and is_empty(move)
;;     moves.append(move)
;;   move = [-2 0]
;;   if moves and is_valid(move) and is_empty(move)
;;     moves.append(move)
;;   move = [-1 -1]
;;   if is_valid(move) and is_theirs(move)
;;     moves.append(move)
;;   move = [-1 1]
;;   if is_valid(move) and is_theirs(move)
;;     moves.append(move)

(def test1-board
     (to-board
       ["r  k    "
        "        "
        " p bq   "
        " Pp  P  "
        "  N     "
        " K      "
        "        "
        "    R   "]))

(comment
  (print-board
    (board-with-moves initial-board
                      (moves-in-dirs initial-board
                                     [3 2] 
                                     BLACK
                                     [[0 1] [-1 0]]
                                     1)))
  (print-board test1-board)
  (defn show-moves [pos]
    (print-board (board-with-moves test1-board
                                   (valid-moves test1-board pos))))
  (show-moves [0 0])
  (show-moves [2 4])
  )
