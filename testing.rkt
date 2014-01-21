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
  ;   - 'shack
  ;   - 'house
  ;   - 'mansion
  ;   - 'castle

; next-tile : symbol -> symbol
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
    [else (error (format "not a tile: ~s~n" cur-tile))]))

(module+ test 
  (check-equal? (next-tile 'grass) 'bush)
  (check-equal? (next-tile 'bush)  'tree)
  (check-equal? (next-tile 'tree)  'shack)
  (check-equal? (next-tile 'shack) 'house)
  (check-equal? (next-tile 'house) 'mansion)
  (check-equal? (next-tile 'mansion) 'castle)
  (check-error (next-tile 'test) "not a tile: test")
  )


; A tile is
;  (make-tile symbol num num boolean)
(define-struct tile ((val) x y (chk #:mutable)) #:transparent)

(define game-board
  (list
   (list (tile 'grass 0 0 #t) (tile 'blank 1 0 #t) (tile 'grass 2 0 #f))
   (list (tile 'blank 0 1 #t) (tile 'grass 1 1 #f) (tile 'blank 2 1 #f))
   (list (tile 'grass 0 2 #f) (tile 'blank 1 2 #f) (tile 'grass 2 2 #f))))

(define board-size (length game-board))

(define board1
  (list
   (list (tile 'grass 0 0 #t) (tile 'grass 1 0 #t) (tile 'blank 2 0 #f))
   (list (tile 'grass 0 1 #t) (tile 'blank 1 1 #f) (tile 'blank 2 1 #f))
   (list (tile 'blank 0 2 #f) (tile 'blank 1 2 #f) (tile 'blank 2 2 #f))))

(define board1b
  (list
   (list (tile 'grass 0 0 #f) (tile 'grass 1 0 #f) (tile 'blank 2 0 #f))
   (list (tile 'grass 0 1 #f) (tile 'blank 1 1 #f) (tile 'blank 2 1 #f))
   (list (tile 'blank 0 2 #f) (tile 'blank 1 2 #f) (tile 'blank 2 2 #f))))

(define board1a
  (list
   (list (tile 'bush  0 0 #t) (tile 'blank 1 0 #t) (tile 'blank 2 0 #f))
   (list (tile 'blank 0 1 #t) (tile 'blank 1 1 #f) (tile 'blank 2 1 #f))
   (list (tile 'blank 0 2 #f) (tile 'blank 1 2 #f) (tile 'blank 2 2 #f))))

;get-tile: list-of-list-of-tiles num num -> tile
; Returns the 'tile' with coordinates ('x','y') on 'board'

(define (get-tile board x y)
  (list-ref (list-ref board y) x))

(module+ test 
  (check-equal? (get-tile board1  0 1) (tile 'grass 0 1 #t))
  (check-equal? (get-tile board1a 0 1) (tile 'blank 0 1 #t))
  )

;on-board : list-of-list-of-tiles num num -> bool
;Checks if a given tile ('x','y') is on the 'board'

(define (on-board? board x y)
  (cond
    [(empty? board) #f]
    [(and (and (< x board-size) (> x -1)) 
          (and (< y board-size) (> y -1))) #t]
    [else #f]))

(module+ test
  (check-equal? (on-board? empty  1 0) #f)
  (check-equal? (on-board? board1  1 0) #t)
  (check-equal? (on-board? board1a 1 8) #f)
  (check-equal? (on-board? board1  -1 0) #f)
  )

; replace-neighbour-row list-of-tiles num num -> list-of-tiles
; Find and replace all visited tiles with blank except ('x', 'y')

(define (replace-neighbour-row row x y)
  (cond
    [(empty? row) empty]
    [(cons? row) 
     (cond 
       [(tile-chk (first row)) 
        (cond 
          [(and (equal? (tile-x (first row)) x)
                (equal? (tile-y (first row)) y)
                (equal? (tile-y (first row)) y))
           (list* (tile (next-tile (tile-val (first row))) x y #t)
                  (replace-neighbour-row (rest row) x y))]
          [else  (list* (tile 'blank 
                              (tile-x (first row)) 
                              (tile-y (first row)) 
                              #t) 
                        (replace-neighbour-row (rest row) x y))])]
       [else (list* (tile (tile-val (first row)) 
                          (tile-x (first row))
                          (tile-y (first row))
                          (tile-chk (first row)))
                    (replace-neighbour-row (rest row) x y))])]))

(module+ test
  (check-equal? (replace-neighbour-row empty 0 0) empty)
  (check-equal? (replace-neighbour-row (list (tile 'grass 0 0 #f) 
                                             (tile 'bush  0 1 #t)
                                             (tile 'bush  0 2 #t)) 0 1)
                (list (tile 'grass 0 0 #f)
                      (tile 'tree  0 1 #t)
                      (tile 'blank 0 2 #t)))
  (check-equal? (replace-neighbour-row (list (tile 'blank 1 0 #f) 
                                             (tile 'bush 1 1 #t)
                                             (tile 'blank 1 2 #t)) 1 1)
                (list (tile 'blank 1 0 #f) 
                      (tile 'tree  1 1 #t)
                      (tile 'blank 1 2 #t)))
  (check-error (replace-neighbour-row (list (tile 'blank 2 0 #f) 
                                            (tile 'blank 2 1 #t)
                                            (tile 'blank 2 2 #t)) 2 2)
               "not a tile: blank"))

;replace-neighbours : list-of-list-of-tiles num num -> list-of-list-of-tiles
;Finds and replaces all neighbours of ('x','y') on 'board' with higher field
;Algo -
;Scan through the entire board, and replace all visited tiles except this-tile
;with 'blank. 
;Replace this tile with 'next-tile'

(define (replace-neighbour board x y)
  (cond
    [(empty? board) empty]
    [(cons? board) 
     (cons (replace-neighbour-row(first board) x y)
           (replace-neighbour (rest board) x y))]))

(module+ test 
  ;(check-equal? (replace-neighbour board1 0 0) board1a)
  ;(check-equal? (replace-neighbour board1 1 0) board1)
  (check-equal? (replace-neighbour 
                 (list (list (tile 'grass 0 0 #t) (tile 'grass 1 0 #t))
                       (list (tile 'grass 0 1 #t) (tile 'blank 1 1 #f))) 0 0)
                (list (list (tile 'bush  0 0 #t) (tile 'blank 1 0 #t))
                      (list (tile 'blank 0 1 #t) (tile 'blank 1 1 #f))))
  )

; valid-neighbour? : list-of-list-of-tile num num symbol -> boolean
; Checks if a given tile is a valid neighbour based on 'val'
(define (valid-neighbour? board x y val)
  (if (on-board? board x y)
      (if (and (symbol=? (tile-val (get-tile board x y)) val)
               (not (tile-chk (get-tile board x y)))
               (not (symbol=? (tile-val (get-tile board x y)) 'blank)))
          #t #f)
      #f))

;count-neighbour :  list-of-list-of-tile num num num -> num
;Returns 'count', the number of relevant neighbouring tiles of ('x','y') 
; that are same
;Algo-
;Increment count
;Mark (x,y) as visited
;Visit all the valid neighbouring tiles(8) and call count-neighbour

(define (count-neighbour board x y c)
  (cond
    [(empty? board) 0]
    [else
     (set! c (add1 c))
     (set-tile-chk! (get-tile board x y) #t)
     (define this-tile (struct-copy tile (get-tile board x y)))
     
     (cond 
       ;north
       [(valid-neighbour? board x (- y 1) (tile-val this-tile))
        (set! c (count-neighbour board x (- y 1) c))c]
       ;south
       [(valid-neighbour? board x (+ y 1) (tile-val this-tile))
        (set! c (count-neighbour board x (+ y 1) c))c]
       ;east
       [(valid-neighbour? board (+ x 1) y (tile-val this-tile))
        (set! c (count-neighbour board (+ x 1) y c))c]
       ;west
       [(valid-neighbour? board (- x 1) y (tile-val this-tile))
        (set! c (count-neighbour board (- x 1) y c))c]
       ;north-east
       [(valid-neighbour? board (+ x 1) (- y 1) (tile-val this-tile))
        (set! c (count-neighbour board (+ x 1) (- y 1) c))c]
       ;north-west
       [(valid-neighbour? board (- x 1) (- y 1) (tile-val this-tile))
        (set! c (count-neighbour board (- x 1) (- y 1) c))c]
       ;south-east
       [(valid-neighbour? board (+ x 1) (+ y 1) (tile-val this-tile))
        (set! c (count-neighbour board (+ x 1) (+ y 1) c))c]
       ;south-west
       [(valid-neighbour? board (- x 1) (- y 1) (tile-val this-tile))
        (set! c (count-neighbour board (- x 1) (- y 1) c))c]
       [else c])]))
 

; reset-check-row : list-of-tiles -> list-of-tiles
; Resets all check fields in 'row' to #f

(define (reset-check-row row)
  (cond
    [(empty? row) row]
    [(cons? row) (cons (tile (tile-val (first row))
                             (tile-x (first row))
                             (tile-y (first row))
                             #f)
                       (reset-check-row (rest row)))]))

(module+ test
  (check-equal? (reset-check-row (cons (tile 'grass 0 0 #t) empty))
               (cons (tile 'grass 0 0 #f) empty))
  (check-equal? (reset-check-row (list (tile 'grass 0 0 #t) 
                                      (tile 'grass 0 0 #t)
                                      (tile 'grass 0 0 #t)))
               (list (tile 'grass 0 0 #f) 
                     (tile 'grass 0 0 #f)
                     (tile 'grass 0 0 #f))))

; reset-check : list of list of tiles -> list of list of tiles
; Resets all check fields in 'board' to #f
  
(define (reset-check board)
  (cond
    [(empty? board) board]
    [(cons? board) (cons (reset-check-row (first board))
                         (reset-check (rest board)))]))

(module+ test 
  (check-equal? (reset-check empty) empty)
  (check-equal? (reset-check 
                 (cons (cons (tile 'grass 0 0 #t) empty) empty)) 
                (cons (cons (tile 'grass 0 0 #f) empty) empty))
  (check-equal? (reset-check 
                 (list (list (tile 'grass 0 0 #t) (tile 'grass 1 0 #f))
                       (list (tile 'blank 0 1 #f) (tile 'blank 1 1 #t))))
                (list (list (tile 'grass 0 0 #f) (tile 'grass 1 0 #f))
                      (list (tile 'blank 0 1 #f) (tile 'blank 1 1 #f))))
  )

; collapse-board : list-of-list-of-tile -> void
; Collapse all possible 3 or more occurances of 'val' into next 'val' in 'board'

(define (collapse-board)
  (for-each 
   (lambda (lot)
     (for-each
      (lambda (tile)
        (set! game-board (reset-check game-board))
        (if (not (symbol=? (tile-val tile) 'blank))
            (if (> (count-neighbour game-board
                                    (tile-x tile)
                                    (tile-y tile)
                                    0)
                   2)
                (set! game-board 
                      (replace-neighbour game-board (tile-x tile) (tile-y tile)))
                empty)
            empty))
      lot))
   game-board))

(collapse-board)

;collapse-tile : list-of-list-of-tile num num -> list-of-list-of-tile
;Collapses all neighbours of ('x','y') based on 'val' of 'tile'('x','y') in 'board'

;Algo-
;check all neighbours of xy
;if xy is blank, return board
;if (> 1)neighbours are the same as xy, replace them with blank
;   and replace xy with next-tile
;else return board

;(define (collapse-tile board x y)
;  (cond
;    [(symbol=? (tile-val (get-tile board x y)) 'blank) board]
;    [(> (count-neighbour board x y 0) 1) (replace-neighbour board x y)]
;    [else board]))
