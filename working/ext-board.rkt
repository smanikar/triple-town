#lang racket
(require rackunit)
(require test-engine/racket-tests)
;(require math/base)
;(require racket/math)

; Task for 1/22
; function that takes a move and put it on the board and makes a collapse

; A tile is either
;   - 'blank 
;   - 'grass
;   - 'bush
;   - 'tree
;   - 'shack
;   - 'house
;   - 'mansion
;   - 'castle

;next-tile : symbol -> symbol
; Gets the next tile of cur-tile
(define (next-tile cur-tile)
  (cond
    ;[(symbol=? cur-tile 'blank) 'grass]
    [(symbol=? cur-tile 'grass) 'bush]
    [(symbol=? cur-tile 'bush)  'tree]
    [(symbol=? cur-tile 'tree)  'shack]
    [(symbol=? cur-tile 'shack) 'house]
    [(symbol=? cur-tile 'house) 'mansion]
    [(symbol=? cur-tile 'mansion) 'castle]
    [(symbol=? cur-tile 'mansion) 'floating-castle]
    [(symbol=? cur-tile 'mansion) 'triple-castle]
    [else (error (format "not a tile: ~s~n" cur-tile))]))

;next-tile-ex : symbol -> symbol
; Gets the next tile of cur-tile 
(define (next-tile-ex cur-tile)
  (cond
    ;[(symbol=? cur-tile 'blank) 'grass]
    [(symbol=? cur-tile 'tombstone) 'church]
    [(symbol=? cur-tile 'church)  'cathedral]
    [(symbol=? cur-tile 'cathedral)  'chest]
    [(symbol=? cur-tile 'chest) 'large-chest]
    [(symbol=? cur-tile 'large-chest) 'rock]
    [(symbol=? cur-tile 'rock) 'mountain]
    [else (error (format "not a tile: ~s~n" cur-tile))]))


;A store house an tile to store

(define store-house empty)
;A list of all possible inputs
(define input-list (list 'grass 'bush 'tree 'hut 'crystal 'imperial-bot 'bear 'ninja-bear))

;gen:input : none -> symbol
; Generates a symbol from 0 to 7 based on weightes

(define (gen-input)
  (let ([n (+ (random 100) 1)])
    (cond
      [(<= n 61) 
       (list-ref input-list 0)]
      [(and (> n 61) (<= n 76))
       (list-ref input-list 1)]
      [(and (> n 76) (<= n 78))
       (list-ref input-list 2)]
      [(and (> n 78) (<= n 79))
       (list-ref input-list 3)]
      [(and (> n 79) (<= n 81))
       (list-ref input-list 4)]
      [(and (> n 81) (<= n 84))
       (list-ref input-list 5)]
      [(and (> n 84) (<= n 99))
       (list-ref input-list 6)]
      [(and (> n 99) (<= n 100))
       (list-ref input-list 4)])))
      ;[else (error "random number")])))

; A tile is (make-tile symbol num num)
(define-struct tile (v x y) #:transparent)

(define board-size 3)

;get-tile: boards num num -> tile
; Returns the 'tile' with coordinates ('x','y') on 'board'

(define (get-tile board x y)
  (list-ref (list-ref board y) x))

;on-board? : num num -> bool
; Checks if a given tile ('x','y') is on the 'board'

(define (on-board? x y)
  (if (and (and (< x board-size) (> x -1)) 
           (and (< y board-size) (> y -1)))
      #t
      #f))

;valid-neighbour? : board num num symbol list -> boolean
; Checks if a given tile is a valid neighbour based on 'v'

(define (valid-neighbour? board x y v l)
  (if (on-board? x y)
      (let ([t (get-tile board x y)])
        (if (and (symbol=? (tile-v t) v)
                 (false? (member t l)))
            #t 
            #f))
      #f))

; replace-row : list num val -> list
;  Replces one item in a list (with 0-based indexing)

(define (replace-row r x v)
  (cond
    [(empty? r) empty]
    [(cons? r) (if (= x 0)
                   (cons (tile v 
                               (tile-x (first r)) 
                               (tile-y (first r))) 
                         (rest r))
                   (cons (first r)
                         (replace-row (rest r) (- x 1) v)))]))

; replace : board num num val -> board
;  Replaces one item in the board

(define (replace b x y v)
  (cond
    [(empty? b) empty]
    [(cons? b) (if (= y 0)
                   (cons (replace-row (first b) x v)
                         (rest b))
                   (cons (first b) (replace (rest b) x (- y 1) v)))]))

;count-neighbour :  
; board num num symbol list -> (list boolean [board-or-#f], num)

(define (count-neighbour b x y v l)
  (if (valid-neighbour? b x y v l)
      (let* ([t (get-tile b x y)]
             [r1 (count-neighbour b x (sub1 y) v (cons t l))]               ;north
             [r2 (count-neighbour (car r1) x (add1 y) v (cons t l))]        ;south
             [r3 (count-neighbour (car r2) (add1 x) y v (cons t l))]        ;east
             [r4 (count-neighbour (car r3) (sub1 x) y v (cons t l))])       ;west
        (list (car r4) (+ 1 (cadr r1) (cadr r2) (cadr r3) (cadr r4))))
      (list b 0)))

(define (replace-neighbours b x y v l)
  (if (valid-neighbour? b x y v l)
      (let* ([t (get-tile b x y)]
             [b0 (replace b x y 'blank)]
             [b1 (replace-neighbours b0 x (sub1 y) v (cons t l))]           ;north
             [b2 (replace-neighbours b1 x (add1 y) v (cons t l))]           ;south
             [b3 (replace-neighbours b2 (add1 x) y v (cons t l))]           ;east
             [b4 (replace-neighbours b3 (sub1 x) y v (cons t l))])          ;west
        b4)
      b))

; place-tile : tile, boards -> boolean [board-or-#f]
;  Places on board and returns if place-tile was successful
(define (place-tile b x y v)
  (if (on-board? x y)
      (if (symbol=? (tile-v (get-tile b x y)) 'blank)
          (replace b x y v)
          #f)
      #f))

; collapse : tile, boards -> boolean [board-or-#f]
;  Place tile, collapse board by replacing all neighbours

(define (collapse b x y v)
  (let ([b0 (place-tile b x y v)])
    (if b0
        (let ([res (count-neighbour b0 x y v empty)])
          (if (> (cadr res) 2)
              (let ([b2 (replace-neighbours (car res) x y v empty)])
                (if b2
                    (replace b2 x y (next-tile v))
                    b2))
              (car res)))
        b0)))

(define (move b)
  (display-board b)
  (let ([inp (gen-input)])
    (printf "New tile - ~a\n" inp)
    (printf "Enter (x,y) - \n")
    (printf "Collapsing...\n")
    (let ([b1 (collapse b (read) (read) inp)])
      (if b1 (display-board b1) #f))))

(define (display-board-row row)
  (cond
    [(empty? row) empty]
    [(cons? row)
     (printf " ~a " (tile-v (first row)))
     (display-board-row (rest row))]))

(define (display-board board)
  (cond
    [(empty? board) empty]
    [(cons? board)
     (display-board-row (first board))
     (printf "\n")
     (display-board (rest board))]))

(define b1
  (list
   (list (tile 'grass 0 0) (tile 'grass 1 0) (tile 'blank 2 0))
   (list (tile 'grass  0 1) (tile 'shack 1 1) (tile 'blank 2 1))
   (list (tile 'grass 0 2) (tile 'blank 1 2) (tile 'grass 2 2))))

(define b2
  (list
   (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'blank 2 0))
   (list (tile 'blank 0 1) (tile 'shack 1 1) (tile 'blank 2 1))
   (list (tile 'blank 0 2) (tile 'bush  1 2) (tile 'blank 2 2))))

(define (test-func)
  (display-board b1)
  (printf "Collapsing ...\n")
  (let
      ([b2 (collapse b1 1 2 'grass)])
    (if b2
        (display-board b2)
        (printf "Failed \n"))))

(move (move (move b1)))
