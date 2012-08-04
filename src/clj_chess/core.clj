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

(defn other-color [color] (if (= color WHITE) BLACK WHITE))

; convert a board (in some known format) to the representation used
; throughout the program.
(defn to-board [board]
     (cond (and (vector? board) (string? (board 0)))
           (vec (map vec board))))

(defn to-picture [board]
  (string/join "\n" (map (partial apply str) board)))

(defn print-board [board] (println (to-picture board)))

(defn with-piece [piece board pos]
  (assoc-in board pos piece))

(defn piece-at [board pos]
  (get-in board pos INVALID))

;;; refactor: rename mark-poses
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


;;; refactor:
;;;    use nil for INVALID
;;;    terms
;;;      "board" (program representation of chess board)
;;;      "pos" (square)
;;;      "value" (what's in a square)
;;;      "piece" non-empty value
;;:
;;;    could do these as (val-empty? [val]) fns and then build
;;;    pos-empty? etc out of those.

(defn pos-empty? [board pos]
  (= (piece-at board pos) EMPTY))

(defn pos-valid? [board pos]
  (not= (piece-at board pos) INVALID))

(defn pos-piece? [board pos]
  (#{ROOK KNIGHT BISHOP QUEEN KING PAWN}
   (kind-of (piece-at board pos))))

(defn pos-enemy? [board color pos]
  (and (pos-piece? board pos)
       (not= color (color-of (piece-at board pos)))))

(defn pos-own? [board color pos]
  (and (pos-piece? board pos)
       (not (pos-enemy? board color pos))))

(defn piece-poses [board]
  (for [rank (range 8)
        file (range 8)
        :when (pos-piece? board [rank file])]
       [rank file]))

(defn color-poses [board color]
  (filter #(= color (color-of (piece-at board %))) (piece-poses board)))

(defn moves-in-dirs [board pos color dirs dist]
  (apply concat
         (for [dir dirs]
              (loop [cur-pos (add-pair pos dir)
                     moves []]
                    (let [piece-there (piece-at board cur-pos)
                          new-moves (conj moves cur-pos)]
                      (cond (= piece-there INVALID)
                              moves
                            (not= piece-there EMPTY)
                            (if (= color (color-of piece-there))
                              moves
                              new-moves)
                            :else
                              (recur (add-pair cur-pos dir) new-moves)))))))

(defn moves-direct [board pos color offsets]
  (filter #(and (pos-valid? board %)
                (not (pos-own? board color %)))
          (map #(add-pair pos %) offsets)))

; positions that color could capture, were there a piece of the other
; color there.
(defn poses-in-check [board color]
  (set (apply concat (map #(valid-captures board %) (color-poses board color)))))

;; assumes board is oriented such that current player's home rank is 7
(defn valid-captures [board pos]
  (let [piece (piece-at board pos)
        color (color-of piece)
        kind (kind-of piece)]
    (cond (= kind ROOK)
            (moves-in-dirs board pos color STRAIGHT-DIRS 8)
          (= kind BISHOP)
            (moves-in-dirs board pos color DIAGONAL-DIRS 8)
          (= kind QUEEN)
            (moves-in-dirs board pos color ALL-DIRS 8)
          (= kind PAWN)
              ; diagonal moves
              (let [poses (map #(add-pair pos %) [[-1 -1] [-1 1]])]
                (filter #(and (pos-valid? board %)
                              (pos-enemy? board color %))
                        poses))
           (= kind KNIGHT)
             (moves-direct board pos colorg
                           [[1 2] [2 1]
                            [-1 2] [2 -1]
                            [-1 -2] [-2 -1]
                            [1 -2] [-2 1]])
           (= kind KING)
             (moves-direct board pos color ALL-DIRS))))

(defn valid-moves [board pos]
   (let [piece (piece-at board pos)
         color (color-of piece)
         kind (kind-of piece)
         capture-moves (valid-captures board pos)]
     (cond (= kind PAWN)
             (concat capture-moves
                     ; forward moves
                     (let [pos1 (add-pair pos [-1 0])
                           pos2 (add-pair pos [-2 0])
                           check (fn [new-pos] (and (pos-valid? board new-pos)
                                                    (pos-empty? board new-pos)))
                           ok1 (check pos1)
                           ok2 (and ok1 (= (pos 0) 6) (check pos2))]
                       (remove nil? [(when ok1 pos1) (when ok2 pos2)])))
           (= kind KING)
             (clojure.set/difference (set capture-moves) (poses-in-check board (other-color color)))
           :else
             capture-moves)))

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

  (print-board (board-with-moves test1-board
                                 (poses-in-check test1-board WHITE)))
  )
