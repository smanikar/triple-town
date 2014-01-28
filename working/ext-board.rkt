#lang racket
(require rackunit)
(require test-engine/racket-tests)

; Task for 1/22
; function that takes a move and put it on the board and makes a collapse

; A tile is either
;   - 'blank 
;   - 'grass
;   - 'bush
;   - 'tree
;   - 'house
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
    [(symbol=? cur-tile 'tree)  'hut]
    [(symbol=? cur-tile 'hut) 'house]
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
  (let ([n (random 100)])
    (cond
      [(< n 61) (list-ref input-list 0)]
      [(< n 76) (list-ref input-list 1)]
      [(< n 78) (list-ref input-list 2)]
      [(< n 79) (list-ref input-list 3)]
      [(< n 81) (list-ref input-list 4)]
      [(< n 84) (list-ref input-list 5)]
      [(< n 99) (list-ref input-list 6)]
      [(< n 100) (list-ref input-list 7)]
      [else (error "random number")])))

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
  (and (and (< x board-size) (> x -1)) 
       (and (< y board-size) (> y -1))))

;valid-neighbour? : board num num symbol list -> boolean
; Checks if a given tile is a valid neighbour based on 'v'

(define (valid-neighbour? board x y v l)
  (and (on-board? x y)
       (let ([t (get-tile board x y)])
         (and (symbol=? (tile-v t) v)
              (false? (member t l))))))

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

;count-neighbours :  
; board num num symbol list -> (list [list-or-#f], num)

(define (count-neighbours b x y v l)
  (cond [(empty? b) (list #f l)]
        [(cons? b)
         (if (valid-neighbour? b x y v l)
             (let* ([t (get-tile b x y)]
                    [l1 (count-neighbours b x (sub1 y) v (cons t l))]               ;north
                    [l2 (count-neighbours b x (add1 y) v (cons t (car l1)))]        ;south
                    [l3 (count-neighbours b (add1 x) y v (cons t (car l2)))]        ;east
                    [l4 (count-neighbours b (sub1 x) y v (cons t (car l3)))])       ;west
               (list (car l4) (+ 1 (cadr l1) (cadr l2) (cadr l3) (cadr l4))))
             (list l 0))]))

;replace-neighbours :
; board num num symbol list -> (list boolean [board-or-#f], list)

(define (replace-neighbours b x y v l)
  (cond
    [(empty? b) (list #f l)]
    [(cons? b)
     (if (valid-neighbour? b x y v l)
         (let* ([t (get-tile b x y)]
                [b0 (replace b x y 'blank)]
                [l1 (replace-neighbours b0 x (sub1 y) v (cons t l))]                    ;north
                [l2 (replace-neighbours (car l1) x (add1 y) v (cons t (cadr l1)))]      ;south
                [l3 (replace-neighbours (car l2) (add1 x) y v (cons t (cadr l2)))]      ;east
                [l4 (replace-neighbours (car l3) (sub1 x) y v (cons t (cadr l3)))])     ;west
           l4)
         (list b l))]))

; place-tile : tile, boards -> boolean [board-or-#f]
;  Places on board and returns if place-tile was successful
(define (place-tile b x y v)
  (cond 
    [(empty? b) #f]
    [(cons? b)
     (and (on-board? x y)
          (and (symbol=? (tile-v (get-tile b x y)) 'blank)
               (replace b x y v)))]))

; collapse : tile, boards -> boolean [board-or-#f]
;  Place tile, collapse board by replacing all neighbours

(define (collapse b x y v)
  (let ([b1 (place-tile b x y v)])
    (let ([res (count-neighbours b1 x y v empty)])
      (if (> (cadr res) 2)
          (replace (car (replace-neighbours b1 x y v empty)) x y (next-tile v))
          b1))))

;move : board -> boolean
; Displays board, reads new tile, collapses board, displays board

(define (move b)
  (cond
    [(empty? b) #f]
    [(cons? b)
     (display-board b)
     (let ([inp (gen-input)])
       (printf "New tile - ~a\n" inp)
       (printf "Enter (x,y) - \n")
       (printf "Collapsing...\n")
       (display-board (collapse b (read) (read) inp)))]))
       
;display-board-row : list-of-tiles -> list-of-tiles
; Displays a row of tiles

(define (display-board-row row)
  (cond
    [(empty? row) empty]
    [(cons? row)
     (printf " ~a " (tile-v (first row)))
     (display-board-row (rest row))]))

;display-board : board -> board
; Displays a board of tiles

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
   (list (tile 'grass 0 1) (tile 'hut   1 1) (tile 'blank 2 1))
   (list (tile 'grass 0 2) (tile 'blank 1 2) (tile 'grass 2 2))))

(define b2
  (list
   (list (tile 'blank 0 0) (tile 'blank 1 0) (tile 'blank 2 0))
   (list (tile 'blank 0 1) (tile 'hut   1 1) (tile 'blank 2 1))
   (list (tile 'blank 0 2) (tile 'bush  1 2) (tile 'blank 2 2))))

(move b1)

