(ns clj-chess.core
    (:use clojure.repl)
    (:require [clojure.string :as string]))

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

; returns color in mate, or nil
(defn is-mate [board] nil)

(defn board-with-moves [board moves]
  (reduce (partial with-piece \x) board moves))

(defn checked-poses [board color])

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
            (moves-in-dirs board pos color ALL-DIRS 8)
          (= kind PAWN)
            (concat
              ; forward moves
              (let [pos1 (add-pair pos [-1 0])
                    pos2 (add-pair pos [-2 0])
                    check (fn [new-pos] (and (pos-valid? board new-pos)
                                             (pos-empty? board new-pos)))
                    ok1 (check pos1)
                    ok2 (and ok1 (= (pos 0) 6) (check pos2))]
                (remove nil? [(when ok1 pos1) (when ok2 pos2)]))
              ; diagonal moves
              (let [poses (map #(add-pair pos %) [[-1 -1] [-1 1]])]
                (filter #(and (pos-valid? board %)
                              (pos-enemy? board color %))
                        poses))
              )
           (= kind KNIGHT)
             (moves-direct board pos color  
                           [[1 2] [2 1]
                            [-1 2] [2 -1]
                            [-1 -2] [-2 -1]
                            [1 -2] [-2 1]]) 
           (= kind KING)
             (let [in-check (checked-poses board color)])
             (moves-direct board pos color ALL-DIRS))))

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
  )
