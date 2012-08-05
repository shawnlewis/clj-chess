(ns clj-chess.core
    (:use clojure.repl)
    (:require [clojure.string :as string]
              [clojure.set]))

;;; utilties

(defn is-upper [char] (= (string/upper-case char) char))

(defn add-pair [p0 p1]
  [(+ (p0 0) (p1 0))
   (+ (p0 1) (p1 1))])

(defn zip [& args]
  (apply map vector args))


;;; chess

;; Terms:
;;   board: program's representation of a chess board. A vector
;;     of vectors of chars.
;;   square: A location on the board, denoted by [rank file].
;;   val(value): The contents of a square.
;;   piece: A chess piece.

(def WHITE)
(def BLACK)
(def ROOK \r)
(def KNIGHT \n)
(def BISHOP \b)
(def QUEEN \q)
(def KING \k)
(def PAWN \p)
(def EMPTY \space)
(def STRAIGHT-DIRS [[1 0] [-1 0] [0 1] [0 -1]])
(def DIAGONAL-DIRS [[1 1] [1 -1] [-1 -1] [-1 1]])
(def ALL-DIRS (concat STRAIGHT-DIRS DIAGONAL-DIRS))

(defn color-of [piece] (if (is-upper (str piece)) WHITE BLACK))
(defn kind-of [piece]  (nth (string/lower-case piece) 0))

(defn other-color [color] (if (= color WHITE) BLACK WHITE))

; convert a board (in some known format) to the representation used
; throughout the program.
(defn to-board [board]
     (cond (and (vector? board) (string? (board 0)))
           (vec (map vec board))))

(defn to-picture [board]
  (string/join "\n" (map (partial apply str) board)))

(defn print-board [board] (println (to-picture board)))

(defn with-val [val board square]
  (assoc-in board square val))

(defn val-at [board square]
  (get-in board square))

(defn mark-squares [board squares]
  (reduce (partial with-val \x) board squares))

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

(defn val-empty? [val]
  (= val EMPTY))

(defn val-piece? [val]
  (#{ROOK KNIGHT BISHOP QUEEN KING PAWN}
   (kind-of val)))

(defn val-own? [color val]
  (and (val-piece? val)
       (= color (color-of val))))

(defn val-enemy? [color val]
  (and (val-piece? val)
       (not= color (color-of val))))

(defn filter-squares [pred board]
  (for [rank (range 8)
        file (range 8)
        :when (pred (piece-at board [rank file]))]
       [rank file]))

(defn color-squares [board color]
  (filter-squares (partial val-own? color) board))

(defn linear-moves [board square color dirs]
  (apply concat
         (for [dir dirs]
              (loop [cur-square (add-pair square dir)
                     moves []]
                    (let [val-there (val-at board cur-square)
                          new-moves (conj moves cur-square)]
                      (cond (not val-there)
                              moves
                            (not (val-empty? val-there))
                              (if (val-own? color val-there)
                                moves
                                new-moves)
                            :else
                              (recur (add-pair cur-square dir) new-moves)))))))

(defn direct-moves [board square color offsets]
  (filter #(if-let [val (val-at board %)]
                   (not (val-own? color val)))
          (map #(add-pair square %) offsets)))

; squares that color could capture, were there a val of the other
; color there.
(defn squares-in-check [board color]
  (set (apply concat (map #(valid-captures board %) (color-squares board color)))))

;; assumes board is oriented such that current player's home rank is 7
(defn valid-captures [board square]
  (let [val (val-at board square)
        color (color-of val)
        kind (kind-of val)]
    (cond (= kind ROOK)
            (linear-moves board square color STRAIGHT-DIRS)
          (= kind BISHOP)
            (linear-moves board square color DIAGONAL-DIRS)
          (= kind QUEEN)
            (linear-moves board square color ALL-DIRS)
          (= kind PAWN)
              ; diagonal moves
              (let [squares (map #(add-pair square %) [[-1 -1] [-1 1]])]
                (filter #(if-let [val (val-at board %)]
                                 (val-enemy? color val))
                        squares))
           (= kind KNIGHT)
             (direct-moves board square color
                           [[1 2] [2 1]
                            [-1 2] [2 -1]
                            [-1 -2] [-2 -1]
                            [1 -2] [-2 1]])
           (= kind KING)
             (direct-moves board square color ALL-DIRS))))

(defn valid-moves [board square]
   (let [val (val-at board square)
         color (color-of val)
         kind (kind-of val)
         capture-moves (valid-captures board square)]
     (cond (= kind PAWN)
             (concat capture-moves
                     ; forward moves
                     (let [square1 (add-pair square [-1 0])
                           square2 (add-pair square [-2 0])
                           check (fn [new-square]
                                     (if-let [val (val-at new-square)]
                                             (val-empty? val)))
                           ok1 (check square1)
                           ok2 (and ok1 (= (square 0) 6) (check square2))]
                       (remove nil? [(when ok1 square1) (when ok2 square2)])))
           (= kind KING)
             (clojure.set/difference (set capture-moves)
                                     (squares-in-check board (other-color color)))
           :else
             capture-moves)))


;;; Tests

(def test1-board
     (to-board
       ["r  k    "
        "        "
        " ppbq   "
        " Pp  P  "
        "  N n   "
        " K    p "
        "       P"
        "    R   "]))

(defn show-moves [square]
(print-board (mark-squares test1-board
                               (valid-moves test1-board square))))
(comment
  (print-board
    (mark-squares initial-board
                      (linear-moves initial-board
                                     [3 2]
                                     BLACK
                                     [[0 1] [-1 0]]
                                     1)))
  (print-board test1-board)
  (show-moves [0 0])
  (show-moves [2 4])
  (show-moves [2 4])

  ; pawns
  (show-moves [3 5])
  (show-moves [6 7])
  (show-moves [3 1])

  ; knights
  (show-moves [4 2])
  (show-moves [4 4])

  ; kings
  (show-moves [0 3])
  (show-moves [5 1])


  (let [exp-board (to-board ["r  k    "
                             "        "
                             " xxxx   "
                             "xPp xP  "
                             "xxN x   "
                             "xKx x x "
                             "xxxxx  P"
                             "xxxxRxxx"])
        checked-board  (mark-squares
                         test1-board
                         (squares-in-check test1-board WHITE))]
    (print-board checked-board)
    (println (= exp-board checked-board)) )
  )
